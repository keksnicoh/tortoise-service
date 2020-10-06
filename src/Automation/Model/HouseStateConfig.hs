module Automation.Model.HouseStateConfig
  ( HouseStateConfig(..)
  , HasHouseStateConfig(..)
  )
where

import           Core.Internal (Temperature)
import qualified Data.Time     as T

data HouseStateConfig
  = HouseStateConfig
  { delaySensorRead :: Int
  , minTemperature  :: Temperature
  , maxTemperature  :: Temperature
  , maxStatusAge    :: T.NominalDiffTime
  , emergencyDelay  :: IO ()
  }

class HasHouseStateConfig a where
  getHouseStateConfig :: a -> HouseStateConfig
