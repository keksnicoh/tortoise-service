module Automation.Service.EmergencyService where

import           Control.Monad.IO.Class
import           Control.Monad.Reader           ( reader
                                                , MonadReader
                                                )
import qualified Core.State.Model.State        as CSMState
import qualified Core.State.Repository.State   as CSRState
import           Automation.FSM.HouseFSM
import           Automation.Model.HouseStateConfig

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
