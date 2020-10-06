{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Content.Model.Monitor
  ( Monitor(..)
  , MonitorWeather(..)
  , from
  )
where

import           Content.Model.Switch               (Switch, fromCoreSwitch)
import qualified Core.Database.Model.Status         as CDB
import           Core.Internal                      (Humidity, Temperature)
import qualified Core.OpenWeatherMap.Model.Forecast as COM
import qualified Core.State.Model.State             as CST
import           Data.Aeson                         (ToJSON)
import           Data.List                          (genericLength)
import           Data.Maybe                         (catMaybes)
import           Data.Time                          (UTCTime)
import           GHC.Generics                       (Generic)

data MonitorWeather
  = MonitorWeather
    { label       :: String
    , temperature :: Temperature
    , humidity    :: Humidity
    , date        :: UTCTime
    } deriving (Show, Eq, Generic, ToJSON)

data Monitor
  = Monitor
    { date                    :: UTCTime
    , sensorTemperatur        :: Maybe Temperature
    , sensorHumidity          :: Maybe Humidity
    , sensorTemperaturOutside :: Maybe Temperature
    , sensorHumidityOutside   :: Maybe Humidity
    , switchLight1            :: Maybe Switch
    , switchLight2            :: Maybe Switch
    , weather                 :: [MonitorWeather]
    , webcamDate              :: Maybe UTCTime
    , houseState              :: String
    }
  deriving (Show, Eq, Generic, ToJSON)

from :: UTCTime -> [CDB.Status] -> CST.State -> COM.ForecastResult -> Monitor
from date status state forecast = Monitor
  date
  (meanFrom CDB.temperature)
  (meanFrom CDB.humidity)
  (meanFrom CDB.temperatureOutside)
  (meanFrom CDB.humidityOutside)
  (fromCoreSwitch <$> CST.light1 state)
  (fromCoreSwitch <$> CST.light2 state)
  (fromForecast forecast)
  (CST.webcamDate state)
  (show $ CST.houseMonitor state)
 where
  meanFrom getField = (safeMean . catMaybes) (getField <$> status)
  safeMean []            = Nothing
  safeMean (head : tail) = Just $ (head + sum tail) / (1 + genericLength tail)
  fromForecast (COM.ForecastResult _ forecats) =
    mapForecast <$> take 6 forecats
  mapForecast forecast = MonitorWeather
    { label       = renderLabel (COM.weather forecast)
    , temperature = COM.temperature forecast - 273.15
    , humidity    = COM.humidity forecast
    , date        = COM.date forecast
    }
  renderLabel Nothing                                 = "unspecified weather"
  renderLabel (Just (COM.ForecastWeather name descr)) = name <> " - " <> descr
