module Automation.Header where

import           Automation.Free.SimpleController (LightId, LightStatus)
import           Automation.FSM.HouseFSM          (TemperatureSensor)

-- |Service to set a light state for a certain lightId. Note that there
-- is no contract that the light state is actually switched to the desired state.
-- this is because there might be a certain runtime which prevents automatic control
-- of the light switched, e.g. manual light switch state is set.
type ProposeSwitchLight m = LightId -> Bool -> m ()

-- |Service to lock a light. Locking a light should provide a way of rejecting
-- proposed light states
type LockLight m = LightId -> m ()

-- |Retrieves the current status of a light
type GetLightStatus m = LightId -> m LightStatus

-- |generic interface to trigger house controlling when temperature is bound
type ControlHandler m = m ()

-- |generic interface to trigger emergenceny action when the temperature is not bound
type EmergencyHandler m = TemperatureSensor -> m ()

-- |interface to read sensor data
type ReadSensorHandler m = m (Maybe TemperatureSensor)

-- |generic delay effect
type DelayHandler m = m ()
