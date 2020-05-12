{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Automation.HouseState where

import           Control.Monad.IO.Class
import           Control.Monad.Reader           ( reader
                                                , runReaderT
                                                , MonadReader
                                                )
import qualified Core.State.Env                as CSEnv
import qualified Core.State.Model.State        as CSMState
import qualified Core.State.Repository.State   as CSRState
import           Data.IORef                     ( modifyIORef'
                                                , IORef
                                                , newIORef
                                                )
import           Core.FSM
import           Control.Concurrent             ( threadDelay )
import qualified Core.Database.Model.Status    as CDMStatus
import qualified Data.Time                     as T
import           Core.Internal
import qualified Dependencies                  as D
import           Data.Maybe                     ( mapMaybe )

instance (MonadIO m, MonadReader e m, CSEnv.HasState e) => HouseFSM (HouseT m) where
  type State (HouseT m) = HouseState

  initializing = do
    monitor CSMState.MonitorIdle
    return Initializing

  controlled _ sd = do
    monitor CSMState.MonitorOK
    liftIO $ putStrLn "[controlled]"
    return (Controlled sd)

  emergency _ sd = do
    monitor CSMState.MonitorEmergency
    liftIO $ putStrLn "[emergency]"
    return (Emergency sd)

  terminate transition = do
    monitor CSMState.MonitorOff
    return (reasonFrom transition)

  noSensorData _ = do
    monitor CSMState.MonitorIdle
    liftIO $ putStrLn "[noSensorData]"
    return NoSensorData

monitor
  :: (MonadReader a m, CSEnv.HasState a, MonadIO m)
  => CSMState.HouseMonitor
  -> m ()
monitor m = do
  stateIORef <- reader CSEnv.getState
  liftIO $ modifyIORef' stateIORef $ \s -> s { CSMState.houseMonitor = m }

data TerminateReason
  = OutOfContronReason
  | SensorUnavailableReason
  deriving (Show)

-- @todo: how to test this function?
reasonFrom :: TransitionTerminating m -> HouseState Terminating
reasonFrom (EmergencyNotResolved _) = Terminating OutOfContronReason
reasonFrom (SensorUnavailable    _) = Terminating SensorUnavailableReason
reasonFrom (SensorNotInitialized _) = Terminating SensorUnavailableReason

data HouseState s where
  Initializing  ::HouseState Initializing
  Controlled    ::TemperatureSensor -> HouseState Controlled
  Emergency     ::TemperatureSensor -> HouseState Emergency
  Terminating   ::TerminateReason -> HouseState Terminating
  NoSensorData  ::HouseState NoSensorData

instance Show s => Show (HouseState s) where
  show Initializing    = "Initializing"
  show (Controlled  t) = "Controlled " ++ show t
  show (Emergency   t) = "Emergency " ++ show t
  show (Terminating r) = "Terminating " ++ show r
  show NoSensorData    = "NoSensorData"

data TestEnv = TestEnv (IORef CSMState.State) HouseStateConfig

instance CSEnv.HasState TestEnv where
  getState (TestEnv s _) = s

data HouseStateConfig
  = HouseStateConfig
  { delaySensorRead :: Int
  , minTemperature :: Temperature
  , maxTemperature :: Temperature
  , retrySensorRead :: Int
  , maxStatusAge :: T.NominalDiffTime
  , emergencyDelay :: IO ()
  }

class HasHouseStateConfig a where
  getHouseStateConfig :: a -> HouseStateConfig

instance HasHouseStateConfig TestEnv where
  getHouseStateConfig (TestEnv _ c) = c

{-| reads the last defined temperature within a configured time range.
    if a temperature value exists then it will be interpreted to be 
    High, Low or Bound. If no temperature value is available, the 
    result of this function is Nothing. -}
mkReadSensor
  :: (MonadIO m, MonadReader e m, HasHouseStateConfig e, D.HasCurrentTime e)
  => CDMStatus.FetchStatusRepository m
  -> m (Maybe TemperatureSensor)
mkReadSensor fetchStatusRepository = do
  minT         <- reader (minTemperature . getHouseStateConfig)
  maxT         <- reader (maxTemperature . getHouseStateConfig)
  maxStatusAge <- reader (maxStatusAge . getHouseStateConfig)
  now          <- reader D.getCurrentTime >>= liftIO
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

sperk :: IO ()
sperk = do
  ioRef <- newIORef CSMState.initialState

  let config = HouseStateConfig { delaySensorRead = 1000000
                                , minTemperature  = 10
                                , maxTemperature  = 40
                                , retrySensorRead = 5
                                , maxStatusAge    = 10
                                , emergencyDelay  = return ()
                                }
      env         = TestEnv ioRef config
      fsmHandlers = FSMHandlers
        { readSensor      = liftIO $ getLine >>= \case
                              "1" -> return (Just (Bound 17.53))
                              "2" -> return (Just (Low 2))
                              "3" -> return (Just (High 40))
                              "4" -> return Nothing
        , controlTick     = liftIO $ putStrLn "controlling :)"
        , emergencyAction = \_ -> return ()
        , nRetry          = 5
        , delay           = liftIO $ do
                              putStrLn "wait..."
                              threadDelay 1000000
                              putStrLn "continue..."
        }
  runReaderT (runHouseT $ mkFSM fsmHandlers) env
  return ()
