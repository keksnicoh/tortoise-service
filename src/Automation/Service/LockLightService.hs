module Automation.Service.LockLightService
  ( mkLockLight
  )
where

import           Control.Monad.Reader           ( join
                                                , reader
                                                , MonadReader
                                                )

import qualified Dependencies                  as D

import qualified Core.State.Repository.State   as CSRState
import qualified Core.State.Model.State        as CSMState

import           Automation.Header              ( LockLight )
import           Automation.Free.SimpleController

-- |sets locked state for given light to current environmental time
mkLockLight
  :: (MonadReader e m, D.HasCurrentTime e m)
  => CSRState.UpdateState m
  -> LockLight m
mkLockLight updateState lightId = do
  time <- join (reader D.getCurrentTime)
  case lightId of
    LightId1 -> updateState $ \s -> s { CSMState.controlLockDate1 = Just time }
    LightId2 -> updateState $ \s -> s { CSMState.controlLockDate2 = Just time }