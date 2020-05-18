module Automation.Model.HouseStateConfig
  ( HouseStateConfig(..)
  , HasHouseStateConfig(..)
  )
where

import qualified Data.Time                     as T
import           Core.Internal
import           Automation.FSM.Transitions

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
