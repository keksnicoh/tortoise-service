{-# LANGUAGE FlexibleContexts #-}
module Automation.Service.LockLightService
  ( mkLockLight
  )
where

import           Control.Monad.Reader           ( MonadReader )
import           OpenEnv
import qualified Data.Time                     as T
import qualified Core.State.Repository.State   as CSRState
import qualified Core.State.Model.State        as CSMState

import           Automation.Header              ( LockLight )
import           Automation.Free.SimpleController

-- |sets locked state for given light to current environmental time
mkLockLight
  :: (MonadReader e m, Embedded T.UTCTime e m)
  => CSRState.UpdateState m
  -> LockLight m
mkLockLight updateState lightId = do
  time <- embedded
  case lightId of
    LightId1 -> updateState $ \s -> s { CSMState.controlLockDate1 = Just time }
    LightId2 -> updateState $ \s -> s { CSMState.controlLockDate2 = Just time }
