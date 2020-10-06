{-# LANGUAGE FlexibleContexts #-}
module Automation.Service.LockLightService
  ( mkLockLight
  )
where

import           Control.Monad.Reader             (MonadReader)
import qualified Core.State.Model.State           as CSMState
import qualified Core.State.Repository.State      as CSRState
import qualified Data.Time                        as T
import           OpenEnv                          (Embedded, embedded)

import           Automation.Free.SimpleController (LightId (LightId1, LightId2))
import           Automation.Header                (LockLight)

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
