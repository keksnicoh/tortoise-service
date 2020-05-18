module Automation.Model.SimpleHandlerConfig where

import qualified Data.Time                     as T

import           Core.Internal

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

instance HasSimpleHandlerConfig SimpleHandlerConfig where
  getSimpleHandlerConfig = id
