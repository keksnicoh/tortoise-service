{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase                 #-}

module Automation.FSM.Transitions

where

import           Control.Monad.Reader           ( MonadReader
                                                , (>=>)
                                                )
import Automation.FSM.HouseFSM
import Automation.Header
import OpenEnv
import Automation.Env


type Delay = Int

data FSMHandlers m
  = FSMHandlers
  { readSensor :: ReadSensorHandler m
  , controlTick :: ControlHandler m
  , emergencyAction :: EmergencyHandler m
  , delay :: DelayHandler m
  }

mkFSM
  :: (HouseFSM m, Monad m, MonadReader e m, Provides FSMNRetry e)
  => FSMHandlers m
  -> m (State m Terminating)
mkFSM h = initialize >>= start h

-- |starts finite state machine by reading sensor data. If no sensor data
-- is available, then the fsm is terminated.
start
  :: (HouseFSM m, Monad m, MonadReader e m, Provides FSMNRetry e)
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
  :: (HouseFSM m, Monad m, MonadReader e m, Provides FSMNRetry e)
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
  :: (HouseFSM m, Monad m, MonadReader e m, Provides FSMNRetry e)
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
    nRetry   <- provide
    retryReadSensor h nRetry newState
  onSuccess sd = sensorRead sd . NewSensorData >=> verifySensorData h sd

-- |when sensor data could not be read, n retries are performed. If temperature
-- data is available again, then a transition into HasSensorData is performed.
retryReadSensor
  :: (HouseFSM m, Monad m, MonadReader e m, Provides FSMNRetry e)
  => FSMHandlers m
  -> FSMNRetry
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
  :: (HouseFSM m, Monad m, MonadReader e m, Provides FSMNRetry e)
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
