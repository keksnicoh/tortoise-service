{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Content.Model.Monitor
  ( MonitorSwitch(..)
  , Monitor(..)
  , MonitorWeather(..)
  , from
  )
where

import           Core.Internal
import qualified Core.Database.Model.Status    as CDB
import qualified Core.State.Model.State        as CST
import qualified Core.OpenWeatherMap.Model.Forecast
                                               as COM
import           Data.Aeson
import           GHC.Generics                   ( Generic )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.List                      ( genericLength )
import           Data.Swagger
import           Data.Time

take' :: [a] -> [a]
take' = take 4

data MonitorSwitch
  = MonitorSwitch
    { value :: Bool
    , fixed :: Bool
    }
  deriving (Show, Eq, Generic, ToSchema, ToJSON)

data MonitorWeather
  = MonitorWeather
    { label       :: String
    , temperature :: Temperature
    , humidity    :: Humidity
    , date        :: UTCTime
    } deriving (Show, Eq, Generic, ToSchema, ToJSON)

data Monitor
  = Monitor
    { date :: UTCTime
    , sensorTemperatur :: Temperature
    , sensorHumidity   :: Humidity
    , switchLight1     :: Maybe MonitorSwitch
    , switchLight2     :: Maybe MonitorSwitch
    , weather          :: [MonitorWeather]
    }
  deriving (Show, Eq, Generic, ToSchema, ToJSON)

from :: UTCTime -> NonEmpty CDB.Status -> CST.State -> COM.ForecastResult -> Monitor
from date status state forecast = Monitor date (mean (CDB.temperature <$> status))
                                     (mean (CDB.humidity <$> status))
                                     (fromSwitch <$> CST.light1 state)
                                     (fromSwitch <$> CST.light2 state)
                                     (fromForecast forecast)
 where
  fromSwitch (CST.Manual     value) = MonitorSwitch value True
  fromSwitch (CST.Controlled value) = MonitorSwitch value False
  mean (head :| tail) =
    let stail = take' tail
        p     = stail
    in  (head + sum p) / (1 + genericLength stail)
  fromForecast (COM.ForecastResult _ forecats) = mapForecast <$> take 6 forecats
  mapForecast forecast = MonitorWeather
    { label       = renderLabel (COM.weather forecast)
    , temperature = COM.temperature forecast
    , humidity    = COM.humidity forecast
    , date        = COM.date forecast
    }
  renderLabel Nothing = "Unspecified Weather"
  renderLabel (Just (COM.ForecastWeather name descr)) = name <> " - " <> descr