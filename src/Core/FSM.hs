{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE EmptyDataDeriving          #-}

{- this module is based on the great blog post
   "Finite-State Machines, Part 2: Explicit Typed State Transitions"
   by Oskar Wickström.
   https://wickstrom.tech/finite-state-machines/2017/11/19/finite-state-machines-part-2.html

   todo:
   - how to avoid manual passing of handlers in mkFSM functions?
   - can we avoid the typeclass?
-}
module Core.FSM
  ( mkFSM
  , HouseFSM(..)
  , HouseT(..)
  , TemperatureSensor(..)
  , FSMHandlers(..)
  , TransitionTerminating(..)
  , HasSensorDataEvent(..)
  , TemperatureBoundEvent(..)
  , HasFSMNRetry(..)
  , Initializing
  , HasSensorData
  , TemperatureBound
  , RetrySensor
  , Emergency
  , Terminating
  )
where

import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( reader
                                                , MonadReader
                                                , (>=>)
                                                )
import           Core.Internal                  ( Temperature )

type Delay = Int

-- @todo is it neccessary to carry the temperature in here, or should
--       a product be used instrad? e.g. `(TemperatureSensor, Temperature)`
data TemperatureSensor
  = Bound Temperature
  | Low Temperature
  | High Temperature
 deriving (Show, Eq)

{- phantom types representing the possible states of the FSM -}
data Initializing deriving (Show, Eq)
data HasSensorData deriving (Show, Eq)
data TemperatureBound deriving (Show, Eq)
data RetrySensor deriving (Show, Eq)
data Emergency deriving (Show, Eq)
data Terminating deriving (Show, Eq)

{- valid states are modeled by a type-class approach using phantom types defined above

                                ................
                                : Initializing :----X terminate SensorNotInitialized
                                :..............:
                                     |                                     X terminate
                                     |                                     | SensorNotInitialized
          sensorRead                 | sensorRead SensorReadable           |
          RecoveredSensorData   .....x...........                       ............
            .-------------------x HasSensorData :-----------------------x Emergeny :
            |                   :..........x....:                       :..........:
            |                        |     |                               |
            |                        |     | sensorRead NewSensorData      |
            |                        |     |                               |
            |               verified |     |                               |
            |    TemperatureVerified |     |                               |
        ...............         .....x..............                       |
     .--x RetrySensor x---------: TemperatureBound x-----------------------
     |  :.............:         :..................:   verified TemperatureRecovered
     |      |     |  retry LostSensor
      ------      |
  retry           |
  RetrySensor     X terminate SensorUnavailable

-}
class HouseFSM m where
  type State m :: * -> *

  initialize   ::                                                 m (State m Initializing)
  sensorRead   :: TemperatureSensor -> HasSensorDataEvent m    -> m (State m HasSensorData)
  verified     ::                      TemperatureBoundEvent m -> m (State m TemperatureBound)
  retry        ::                      RetrySensorEvent m      -> m (State m RetrySensor)
  emergency    ::                      State m HasSensorData   -> m (State m Emergency)
  terminate    ::                      TransitionTerminating m -> m (State m Terminating)

-- @todo maybe this should not be defined in this package?
-- |MTL style transformer
newtype HouseT m a = HouseT
  { runHouseT :: m a
  } deriving ( Functor
             , Monad
             , Applicative
             , MonadIO
             , MonadReader e
             )

-- |possible transitions into `RetrySensor` state
data RetrySensorEvent m
  = LostSensor (State m TemperatureBound)
  | Retry (State m RetrySensor)

-- |possible transitions into `HasSensorDataEvent` state
data HasSensorDataEvent m
  = SensorReadable (State m Initializing)
  | NewSensorData (State m TemperatureBound)
  | RecoveredSensorData (State m RetrySensor)

-- |possible transitions into `TemperatureBound` state
data TemperatureBoundEvent m
  = TemperatureVerified (State m HasSensorData)
  | TemperatureRecovered (State m Emergency)

-- |possible transitions into `Terminating` state
data TransitionTerminating m
  = EmergencyNotResolved (State m Emergency)
  | SensorUnavailable (State m RetrySensor)
  | SensorNotInitialized (State m Initializing)

-- |contains handlers which are invoked in different stages of the state machine.
data FSMHandlers m
  = FSMHandlers
  { readSensor :: m (Maybe TemperatureSensor)
  , controlTick :: m ()
  , emergencyAction :: TemperatureSensor -> m ()
  , delay :: m ()
  }

class HasFSMNRetry a where
  getFSMNRetry :: a -> Int

mkFSM
  :: (HouseFSM m, Monad m, MonadReader e m, HasFSMNRetry e)
  => FSMHandlers m
  -> m (State m Terminating)
mkFSM h = initialize >>= start h

-- |starts finite state machine by reading sensor data. If no sensor data
-- is available, then the fsm is terminated.
start
  :: (HouseFSM m, Monad m, MonadReader e m, HasFSMNRetry e)
  => FSMHandlers m
  -> State m Initializing
  -> m (State m Terminating)
start h state = do
  readSensor h >>= \case
    Nothing -> onError state
    Just sd -> onSuccess sd state
 where
  onError = terminate . SensorNotInitialized
  onSuccess sd = sensorRead sd . SensorReadable >=> verifySensorData h sd

-- |verifies that the temperature is bound (verified) or unbound (emergency).
verifySensorData
  :: (HouseFSM m, Monad m, MonadReader e m, HasFSMNRetry e)
  => FSMHandlers m
  -> TemperatureSensor
  -> State m HasSensorData
  -> m (State m Terminating)
verifySensorData h (Bound _) = verified . TemperatureVerified >=> bound h
verifySensorData h sd        = emergency >=> emergencyProgram h sd

-- |temperature is bound, controlTick can be executed. After delay, new sensor
-- data is read which either leads to a transition back into HasSensorData or
-- RetrySensor. 
bound
  :: (HouseFSM m, Monad m, MonadReader e m, HasFSMNRetry e)
  => FSMHandlers m
  -> State m TemperatureBound
  -> m (State m Terminating)
bound h state = do
  controlTick h
  delay h
  readSensor h >>= \case
    Nothing -> onError state
    Just sd -> onSuccess sd state
 where
  onError state = do
    newState <- retry (LostSensor state)
    nRetry   <- reader getFSMNRetry
    retryReadSensor h nRetry newState
  onSuccess sd = sensorRead sd . NewSensorData >=> verifySensorData h sd

-- |when sensor data could not be read, n retries are performed. If temperature
-- data is available again, then a transition into HasSensorData is performed.
retryReadSensor
  :: (HouseFSM m, Monad m, MonadReader e m, HasFSMNRetry e)
  => FSMHandlers m
  -> Int
  -> State m RetrySensor
  -> m (State m Terminating)
retryReadSensor h n state
  | n < 1 = terminate (SensorUnavailable state)
  | otherwise = do
    delay h
    readSensor h >>= \case
      Nothing -> retry (Retry state) >>= retryReadSensor h (n - 1)
      Just sd ->
        sensorRead sd (RecoveredSensorData state) >>= verifySensorData h sd

-- |when the temperture is out of bounds, this procedure is executed
emergencyProgram
  :: (HouseFSM m, Monad m, MonadReader e m, HasFSMNRetry e)
  => FSMHandlers m
  -> TemperatureSensor
  -> State m Emergency
  -> m (State m Terminating)
emergencyProgram h sd state = do
  emergencyAction h sd
  readSensor h >>= \case
    Just sd@(Bound _) -> onSuccess state
    sensor            -> onError state
 where
  onError   = terminate . EmergencyNotResolved
  onSuccess = verified . TemperatureRecovered >=> bound h
