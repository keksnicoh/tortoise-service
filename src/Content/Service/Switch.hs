{-# LANGUAGE DuplicateRecordFields #-}

module Content.Service.Switch where

import           Content.Model.Switch
import           Core.State.Repository.State
import qualified Core.State.Model.State        as CSM
import           Control.Applicative

type SwitchService m = SwitchRequest -> m ()

mkSwitchService :: UpdateState m -> SwitchService m
mkSwitchService updateState switchRequest = updateState modify
 where
  modify state = state
    { CSM.light1 = (toStateSwitch <$> light1 switchRequest) <|> CSM.light1 state
    , CSM.light2 = (toStateSwitch <$> light2 switchRequest) <|> CSM.light2 state
    }
