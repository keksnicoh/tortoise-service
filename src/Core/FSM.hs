{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE EmptyDataDeriving          #-}

{- this module is based on the great blog post
   "Finite-State Machines, Part 2: Explicit Typed State Transitions"
   by Oskar Wickström.
   https://wickstrom.tech/finite-state-machines/2017/11/19/finite-state-machines-part-2.html
-}
module Core.FSM
  ( mkFSM
  , HouseFSM(..)
  , HouseT(..)
  , TemperatureSensor(..)
  , FSMHandlers(..)
  , TransitionTerminating(..)
  , TransitionControlled(..)
  , Initializing
  , Controlled
  , Emergency
  , Terminating
  , NoSensorData
  )
where

import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadReader
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
data Controlled deriving (Show, Eq)
data Emergency deriving (Show, Eq)
data Terminating deriving (Show, Eq)
data NoSensorData deriving (Show, Eq)

{- valid states are modeled by a type-class approach using phantom types defined above
                              ................
                              : NoSensorData :----------------------.
                              :.....x........:                      | terminate
                       noSensorData |        __                     | SensorUnavailable
                                    |       |__| controlled         |
         controlled SensorRecovered |       |    SensorVerified     |
 ................                  ..x......x....         ..........x....
 : Initializing :------------------x Controlled :         : Terminating x-.
 :..............: controlled       :.x..........:         :...........x.: |
        |         StartControlLoop   | controlled                     |   |
        |                            | EmergencyResolved              |   |
        |                  emergency |                                |   |
        |                     .......x.....                           |   |
        |                     : Emergency :___________________________|   |
        |                     :...........:    terminate                  |
        |                                      EmergencyNotResolved       |
        |_________________________________________________________________|
                               terminate SensorNotInitialized
-}
class HouseFSM m where
  type State m :: * -> *

  initializing     ::                                                m (State m Initializing)
  controlled       :: TransitionControlled m -> TemperatureSensor -> m (State m Controlled)
  emergency        :: State m Controlled     -> TemperatureSensor -> m (State m Emergency)
  noSensorData     :: State m Controlled                          -> m (State m NoSensorData)
  terminate        :: TransitionTerminating m                     -> m (State m Terminating)

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

-- |possible transitions into `Controlled` state
data TransitionControlled m
  = StartControlLoop (State m Initializing)
  | SensorVerified (State m Controlled)
  | EmergencyResolved (State m Emergency)
  | SensorRecovered (State m NoSensorData)

-- |possible transitions into `Terminating` state
data TransitionTerminating m
  = EmergencyNotResolved (State m Emergency)
  | SensorUnavailable (State m NoSensorData)
  | SensorNotInitialized (State m Initializing)

-- |contains handlers which are invoked in different stages of the state machine.
data FSMHandlers m
  = FSMHandlers
  { readSensor :: m (Maybe TemperatureSensor)
  , controlTick :: m ()
  , emergencyAction :: TemperatureSensor -> m ()
  , delay :: m ()
  , nRetry :: Int
  }

mkFSM :: (HouseFSM m, Monad m) => FSMHandlers m -> m (State m Terminating)
mkFSM handlers = initializing >>= start
 where
  -- |main loop, if the temperature is bound, then the controlStep is executed.
  -- Otheriwse a transition into emergency state occurs.
  controlLoop sd transition = controlled transition sd >>= case sd of
    (Bound _) -> controlStep
    sd        -> flip emergency sd >=> emergencyProgram sd

  -- |starts finite state machine by reading sensor data. If no sensor data
  -- is available, then the fsm is terminated.
  start = process action onError onSuccess
   where
    action  = readSensor handlers
    onError = terminate . SensorNotInitialized
    onSuccess sd = controlLoop sd . StartControlLoop

  -- |we perform a controlTick within each step, read delayed sensor data and
  -- either goto the beginning of the controlLoop or retry.
  controlStep state = do
    controlTick handlers
    process action onError onSuccess state
   where
    action  = delay handlers >> readSensor handlers
    onError = noSensorData >=> retryReadSensor (nRetry handlers)
    onSuccess sd = controlLoop sd . SensorVerified

  -- |if n is a positive number, read the delayed sensor data and either
  -- goto the beginning of the control loop or repeat this procedure, depending
  -- on whether sensor data exists.
  retryReadSensor n | n < 1     = terminate . SensorUnavailable
                    | otherwise = process action onError onSuccess
   where
    action  = delay handlers >> readSensor handlers
    onError = retryReadSensor (n - 1)
    onSuccess sd = controlLoop sd . SensorRecovered

  -- |when the temperture is out of bounds, this procedure is executed
  emergencyProgram sd state =
    emergencyAction handlers sd >> readSensor handlers >>= \case
      Just sd@(Bound _) -> controlLoop sd (EmergencyResolved state)
      sensor            -> terminate (EmergencyNotResolved state)

-- | helper function to process sensor data
process read onError onSuccess state = read >>= \case
  Nothing -> onError state
  Just sd -> onSuccess sd state
