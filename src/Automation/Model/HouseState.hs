{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE GADTs                #-}

module Automation.Model.HouseState
  ( HouseState(..)
  , reasonFrom
  , TerminateReason(..)
  )
where

import           Automation.FSM.HouseFSM

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
