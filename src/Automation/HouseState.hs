{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE GADTs                #-}

module Automation.HouseState where

import           Control.Monad.IO.Class
import           Control.Monad.Reader           (join,  reader
                                                , MonadReader
                                                )
import qualified Core.State.Env                as CSEnv
import qualified Core.State.Model.State        as CSMState
import qualified Core.State.Repository.State   as CSRState
import           Data.IORef                     ( modifyIORef'
                                                , IORef
                                                )
import           Core.FSM
import qualified Core.Database.Model.Status    as CDMStatus
import qualified Data.Time                     as T
import qualified Dependencies                  as D
import           Data.Maybe                     ( mapMaybe )
import           Automation.Config

instance (MonadIO m, MonadReader e m, CSEnv.HasState e) => HouseFSM (HouseT m) where
  type State (HouseT m) = HouseState

  initialize = do
    monitor CSMState.MonitorOff
    liftIO $ putStrLn "initialize house monitoring"
    return Initializing

  sensorRead sd _ = do
    monitor CSMState.MonitorOK
    liftIO $ putStrLn ("house monitoring: received sensor data - " ++ show sd)
    return (HasSensorData sd)

  verified _ = do
    monitor CSMState.MonitorOK
    liftIO $ putStrLn "house monitoring: temperature verified"
    return TemperatureBound

  retry _ = do
    monitor CSMState.MonitorSensorRetry
    liftIO $ putStrLn "house monitoring: could not read sensor. retry..."
    return RetrySensor

  emergency _ = do
    monitor CSMState.MonitorEmergency
    liftIO $ putStrLn "house monitoring: temperature not bound, emergency"
    return Emergency

  terminate event = do
    monitor CSMState.MonitorOff
    liftIO $ putStrLn "house monitoring: terminating"
    return $ reasonFrom event

monitor
  :: (MonadReader a m, CSEnv.HasState a, MonadIO m)
  => CSMState.HouseMonitor
  -> m ()
monitor m = do
  stateIORef <- reader CSEnv.getState
  liftIO $ modifyIORef' stateIORef $ \s -> s { CSMState.houseMonitor = m }

data TerminateReason
  = OutOfControlReason
  | SensorUnavailableReason
  deriving (Show)

-- @todo: how to test this function?
reasonFrom :: TransitionTerminating m -> HouseState Terminating
reasonFrom (EmergencyNotResolved _) = Terminating OutOfControlReason
reasonFrom (SensorUnavailable    _) = Terminating SensorUnavailableReason
reasonFrom (SensorNotInitialized _) = Terminating SensorUnavailableReason

data HouseState s where
  Initializing     ::HouseState Initializing
  HasSensorData    ::TemperatureSensor -> HouseState HasSensorData
  TemperatureBound ::HouseState TemperatureBound
  RetrySensor      ::HouseState RetrySensor
  Emergency        ::HouseState Emergency
  Terminating      ::TerminateReason -> HouseState Terminating

instance Show s => Show (HouseState s) where
  show Initializing      = "Initializing"
  show (HasSensorData t) = "HasSensorData " ++ show t
  show TemperatureBound  = "TemperatureBound"
  show RetrySensor       = "RetrySensor"
  show Emergency         = "Emergency"
  show (Terminating r)   = "Terminating " ++ show r

data TestEnv = TestEnv (IORef CSMState.State) HouseStateConfig Int

instance CSEnv.HasState TestEnv where
  getState (TestEnv s _ _) = s

instance HasHouseStateConfig TestEnv where
  getHouseStateConfig (TestEnv _ c _) = c

{-| reads the last defined temperature within a configured time range.
    if a temperature value exists then it will be interpreted to be 
    High, Low or Bound. If no temperature value is available, the 
    result of this function is Nothing. -}
mkReadSensor
  :: (MonadIO m, MonadReader e m, HasHouseStateConfig e, D.HasCurrentTime e m)
  => CDMStatus.FetchStatusRepository m
  -> m (Maybe TemperatureSensor)
mkReadSensor fetchStatusRepository = do
  minT         <- reader (minTemperature . getHouseStateConfig)
  maxT         <- reader (maxTemperature . getHouseStateConfig)
  maxStatusAge <- reader (maxStatusAge . getHouseStateConfig)
  now          <- join (reader D.getCurrentTime)
  let ageFilter s = T.diffUTCTime now (CDMStatus.created s) <= maxStatusAge
      interpret [] = Nothing
      interpret (x : _) | x < minT  = Just (Low x)
                        | x > maxT  = Just (High x)
                        | otherwise = Just (Bound x)
      pipeline = interpret . mapMaybe CDMStatus.temperature . filter ageFilter
  pipeline <$> fetchStatusRepository

{-| this action turns off / on the lights when it is startet with a Low / High
    temperature state. it will reset to the original state when
      - the original state was undefined
      - ...                was controlled
    if the light state chang during delay to manual, the reset is skipped as well.  
-}
mkEmergencyAction
  :: (MonadIO m, MonadReader e m, HasHouseStateConfig e)
  => CSRState.GetState m
  -> CSRState.UpdateState m
  -> TemperatureSensor
  -> m ()

mkEmergencyAction _        _           (Bound _) = return ()
mkEmergencyAction getState updateState value     = do
  state <- getState
  delay <- reader (emergencyDelay . getHouseStateConfig)
  let light1     = CSMState.light1 state
      light2     = CSMState.light2 state
      lightState = case value of
        (Low _) -> True
        _       -> False

  -- set the new state
  updateState $ \state -> state
    { CSMState.light1 = Just (CSMState.Controlled lightState)
    , CSMState.light2 = Just (CSMState.Controlled lightState)
    }

  -- wait so the new state can lead to an effect
  liftIO delay

  -- maybe reset to original state
  state' <- getState
  ifNotManual (ifManual (update1 light1) light1) (CSMState.light1 state')
  ifNotManual (ifManual (update2 light2) light2) (CSMState.light2 state')

 where
  update1 v = updateState $ \s -> s { CSMState.light1 = v }
  update2 v = updateState $ \s -> s { CSMState.light2 = v }
  ifNotManual _      (Just (CSMState.Manual _)) = return ()
  ifNotManual action _                          = action

  ifManual action (Just (CSMState.Manual _)) = action
  ifManual _      _                          = return ()
