module Automation.Config
  ( HouseStateConfig(..)
  , HasHouseStateConfig(..)
  )
where

import           Core.FSM
import qualified Data.Time                     as T
import           Core.Internal

data HouseStateConfig
  = HouseStateConfig
  { delaySensorRead :: Int
  , minTemperature :: Temperature
  , maxTemperature :: Temperature
  , retrySensorRead :: Int
  , maxStatusAge :: T.NominalDiffTime
  , emergencyDelay :: IO ()
  }

class HasHouseStateConfig a where
  getHouseStateConfig :: a -> HouseStateConfig

instance HasFSMNRetry HouseStateConfig where
  getFSMNRetry = retrySensorRead
