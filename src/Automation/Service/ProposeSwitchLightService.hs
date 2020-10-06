{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Automation.Service.ProposeSwitchLightService
  ( mkProposeSwitchLight
  )
where

import           Automation.Free.SimpleController (LightId (LightId1, LightId2), LightStatus (LightLocked, LightManual))
import           Automation.Header                (GetLightStatus,
                                                   ProposeSwitchLight)
import qualified Core.State.Model.State           as CSMState
import qualified Core.State.Repository.State      as CSRState

{-| proposes a change of light switch state. dependeing on the state of the
    light switch, the porposed new state might be ignored, (e.g. light is locked). -}
mkProposeSwitchLight
  :: (Monad m)
  => GetLightStatus m
  -> CSRState.UpdateState m
  -> ProposeSwitchLight m
mkProposeSwitchLight getLightStatus updateState lightId isOn = do
  getLightStatus lightId >>= \case
    LightLocked -> return ()
    LightManual -> return ()
    _           -> set lightId $ Just (CSMState.Controlled isOn)
 where
  set LightId1 v = updateState $ \s -> s { CSMState.light1 = v }
  set LightId2 v = updateState $ \s -> s { CSMState.light2 = v }
