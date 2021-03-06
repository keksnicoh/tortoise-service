{-# LANGUAGE DuplicateRecordFields #-}

module Content.Service.SwitchService
  ( SwitchService
  , mkSwitchService
  )
where

import           Content.Model.Switch        (SwitchRequest (light1, light2),
                                              toCoreSwitch)
import           Control.Applicative         (Alternative ((<|>)))
import qualified Core.State.Model.State      as CSM
import           Core.State.Repository.State (UpdateState)

type SwitchService m = SwitchRequest -> m ()

-- |creates a SwitchService m to update light switch states
mkSwitchService :: UpdateState m -> SwitchService m
mkSwitchService updateState switchRequest = updateState modify
 where
  modify state = state
    { CSM.light1 = (toCoreSwitch <$> light1 switchRequest) <|> CSM.light1 state
    , CSM.light2 = (toCoreSwitch <$> light2 switchRequest) <|> CSM.light2 state
    }
