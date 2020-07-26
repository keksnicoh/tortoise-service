{-# LANGUAGE FlexibleContexts #-}
module Automation.Service.GetLightStatusService
  ( mkGetLightStatus
  )
where

import           Control.Monad.Reader           ( MonadReader )
import           OpenEnv
import qualified Data.Time                     as T
import qualified Core.State.Repository.State   as CSRState
import qualified Core.State.Model.State        as CSMState
import           Automation.Header              ( GetLightStatus )
import           Automation.Model.SimpleHandlerConfig
import           Automation.Free.SimpleController

-- |returns the current state of a light switch used to control the light
mkGetLightStatus
  :: (MonadReader e m, Provides SimpleHandlerConfig e, Embedded T.UTCTime e m)
  => CSRState.GetState m
  -> GetLightStatus m
mkGetLightStatus getState lightId = do
  lockDuration <- lockDuration <$> provide
  time         <- embedded

  dispatch time lockDuration . getByLightId lightId <$> getState
 where
  dispatch time lockDuration (Just state, Just date) =
    if T.diffUTCTime time date > lockDuration
      then mapLight state
      else LightLocked
  dispatch _ _ (Just state, Nothing) = mapLight state
  dispatch _ _ (Nothing   , _      ) = LightUndefined

  mapLight (CSMState.Controlled False) = LightOff
  mapLight (CSMState.Manual     False) = LightManual
  mapLight (CSMState.Controlled True ) = LightOn
  mapLight (CSMState.Manual     True ) = LightManual

  getByLightId LightId1 s = (CSMState.light1 s, CSMState.controlLockDate1 s)
  getByLightId LightId2 s = (CSMState.light2 s, CSMState.controlLockDate2 s)
