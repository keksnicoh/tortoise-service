{-# LANGUAGE DuplicateRecordFields #-}

module Content.Service.Switch where

import           Content.Model.SwitchRequest
import           Core.State.Repository.State
import qualified Core.State.Model.State        as CSM

type SwitchService m = SwitchRequest -> m ()

mkSwitchService :: UpdateState m -> SwitchService m
mkSwitchService updateState switchRequest = updateState modify
 where
  modify state = state
    { CSM.light1 = switch (CSM.light1 state) (light1 switchRequest)
    , CSM.light2 = switch (CSM.light2 state) (light2 switchRequest)
    }
  switch (Just (CSM.Controlled a)) Nothing  = Just (CSM.Controlled a)
  switch (Just (CSM.Manual     a)) Nothing  = Just (CSM.Controlled a)
  switch _                         newState = CSM.Manual <$> newState
