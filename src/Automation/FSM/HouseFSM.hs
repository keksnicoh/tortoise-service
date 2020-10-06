{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{- this module is based on the great blog post
   "Finite-State Machines, Part 2: Explicit Typed State Transitions"
   by Oskar Wickström.
   https://wickstrom.tech/finite-state-machines/2017/11/19/finite-state-machines-part-2.html

   todo:
   - how to avoid manual passing of handlers in mkFSM functions?
   - can we avoid the typeclass?
-}
module Automation.FSM.HouseFSM
  ( HouseFSM(..)
  , TemperatureSensor(..)
  , TransitionTerminating(..)
  , HasSensorDataEvent(..)
  , TemperatureBoundEvent(..)
  , RetrySensorEvent(..)
  , Initializing
  , HasSensorData
  , TemperatureBound
  , RetrySensor
  , Emergency
  , Terminating
  )
where

import           Core.Internal (Temperature)

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
