{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Automation.Controller where

import qualified Data.Time                     as T
import qualified Dependencies                  as D
import           Control.Monad.Reader           ( reader
                                                , when
                                                , MonadReader
                                                )
import qualified Core.State.Repository.State   as CSRState
import qualified Core.State.Model.State        as CSMState
import           Control.Monad                  ( join )
import           Automation.Header
import           Automation.Model.SimpleHandlerConfig
import           Core.FreeSimpleController

mkGetLightStatus
  :: (MonadReader e m, HasSimpleHandlerConfig e, D.HasCurrentTime e m)
  => CSRState.GetState m
  -> GetLightStatus m
mkGetLightStatus getState lightId = do
  lockDuration <- lockDuration <$> reader getSimpleHandlerConfig
  time         <- join (reader D.getCurrentTime)
  dispatch time lockDuration . getByLightId lightId <$> getState
 where
  getByLightId LightId1 s = (CSMState.light1 s, CSMState.controlLockDate1 s)
  getByLightId LightId2 s = (CSMState.light2 s, CSMState.controlLockDate1 s)
  dispatch time lockDuration (Just state, Just date) =
    if T.diffUTCTime date time > lockDuration
      then mapLight state
      else LightLocked
  dispatch _ _ (Just state, Nothing) = mapLight state
  dispatch _ _ (Nothing   , _      ) = LightUndefined
  mapLight (CSMState.Controlled False) = LightOff
  mapLight (CSMState.Controlled False) = LightOff
  mapLight (CSMState.Manual     True ) = LightOn
  mapLight (CSMState.Manual     True ) = LightOn

mkProposeSwitchLight
  :: (MonadReader e m, HasSimpleHandlerConfig e, D.HasCurrentTime e m)
  => CSRState.GetState m
  -> CSRState.UpdateState m
  -> ProposeSwitchLight m
mkProposeSwitchLight getState updateState lightId isOn = do
  lockDuration <- lockDuration <$> reader getSimpleHandlerConfig
  time         <- join (reader D.getCurrentTime)
  getByLightId lightId <$> getState >>= \case
    (Just (CSMState.Manual _), _) -> return ()
    (_, Just date) -> when (T.diffUTCTime date time > lockDuration)
      $ set lightId time (Just (CSMState.Controlled isOn))
    (_, Nothing) -> set lightId time $ Just (CSMState.Controlled isOn)
 where
  getByLightId LightId1 s = (CSMState.light1 s, CSMState.controlLockDate1 s)
  getByLightId LightId2 s = (CSMState.light2 s, CSMState.controlLockDate1 s)

  set LightId1 t v = updateState
    $ \s -> s { CSMState.light1 = v, CSMState.controlLockDate1 = Just t }
  set LightId2 t v = updateState
    $ \s -> s { CSMState.light2 = v, CSMState.controlLockDate2 = Just t }

mkLockLight
  :: (MonadReader e m, HasSimpleHandlerConfig e, D.HasCurrentTime e m)
  => CSRState.UpdateState m
  -> LockLight m
mkLockLight updateState lightId = do
  lockDuration <- lockDuration <$> reader getSimpleHandlerConfig
  time         <- join (reader D.getCurrentTime)
  case lightId of
    LightId1 -> updateState $ \s -> s { CSMState.controlLockDate1 = Just time }
    LightId2 -> updateState $ \s -> s { CSMState.controlLockDate2 = Just time }

