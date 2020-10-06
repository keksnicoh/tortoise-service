{-# LANGUAGE FlexibleContexts #-}
module Automation.Service.GetLightStatusService
  ( mkGetLightStatus
  )
where

import           Automation.Free.SimpleController     (LightId (LightId1, LightId2),
                                                       LightStatus (LightLocked, LightManual, LightOff, LightOn, LightUndefined))
import           Automation.Header                    (GetLightStatus)
import           Automation.Model.SimpleHandlerConfig (SimpleHandlerConfig (lockDuration))
import           Control.Monad.Reader                 (MonadReader)
import qualified Core.State.Model.State               as CSMState
import qualified Core.State.Repository.State          as CSRState
import qualified Data.Time                            as T
import           OpenEnv                              (Embedded, Provides,
                                                       embedded, provide)

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
