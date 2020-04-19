{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Content.Model.Monitor
  ( MonitorSwitch(..)
  , Monitor(..)
  , from
  )
where

import           Core.Internal
import qualified Core.Database.Model.Status    as CDB
import qualified Core.State.Model.State        as CST
import           Data.Aeson
import           GHC.Generics                   ( Generic )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.List                      ( genericLength )
import           Data.Swagger

take' :: [a] -> [a]
take' = take 4

data MonitorSwitch
  = MonitorSwitch
    { value :: Bool
    , fixed :: Bool
    }
  deriving (Show, Eq, Generic, ToSchema, ToJSON)

data Monitor
  = Monitor
    { sensorTemperatur :: Temperature
    , sensorHumidity :: Humidity
    , switchLight1 :: Maybe MonitorSwitch
    , switchLight2 :: Maybe MonitorSwitch
    }
  deriving (Show, Eq, Generic, ToSchema, ToJSON)

from :: NonEmpty CDB.Status -> CST.State -> Monitor
from status state = Monitor (mean (CDB.temperature <$> status))
                            (mean (CDB.humidity <$> status))
                            (fromSwitch <$> CST.light1 state)
                            (fromSwitch <$> CST.light2 state)
 where
  fromSwitch (CST.Manual     value) = MonitorSwitch value True
  fromSwitch (CST.Controlled value) = MonitorSwitch value False
  mean (head :| tail) =
    let stail = take' tail
        p     = stail
    in  (head + sum p) / (1 + genericLength stail)


