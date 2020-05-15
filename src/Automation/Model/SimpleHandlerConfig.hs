module Automation.Model.SimpleHandlerConfig where

import Core.Internal
import qualified Data.Time as T

data TRange
  = TRange
  { lowT :: Temperature
  , highT :: Temperature
  }

data SimpleHandlerConfig
  = SimpleHandlerConfig
  { l1TRange :: TRange
  , l2TRange :: TRange
  , lockDuration :: T.NominalDiffTime
  }

class HasSimpleHandlerConfig e where
  getSimpleHandlerConfig :: e -> SimpleHandlerConfig
