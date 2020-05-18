{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Automation.Service.ProposeSwitchLightService
  ( mkProposeSwitchLight
  )
where

import qualified Dependencies                  as D
import           Control.Monad.Reader           ( reader
                                                , MonadReader
                                                )
import qualified Core.State.Repository.State   as CSRState
import qualified Core.State.Model.State        as CSMState
import           Control.Monad                  ( join )
import           Automation.Header              ( GetLightStatus
                                                , ProposeSwitchLight
                                                )
import           Automation.Free.SimpleController

{-| proposes a change of light switch state. dependeing on the state of the
    light switch, the porposed new state might be ignored, (e.g. light is locked). -}
mkProposeSwitchLight
  :: (MonadReader e m, D.HasCurrentTime e m)
  => GetLightStatus m
  -> CSRState.UpdateState m
  -> ProposeSwitchLight m
mkProposeSwitchLight getLightStatus updateState lightId isOn = do
  getLightStatus lightId >>= \case
    LightLocked -> return ()
    LightManual -> return ()
    _           -> do
      time <- join (reader D.getCurrentTime)
      set lightId time $ Just (CSMState.Controlled isOn)
 where
  set LightId1 t v = updateState
    $ \s -> s { CSMState.light1 = v, CSMState.controlLockDate1 = Just t }
  set LightId2 t v = updateState
    $ \s -> s { CSMState.light2 = v, CSMState.controlLockDate2 = Just t }
