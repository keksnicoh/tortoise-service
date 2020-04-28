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
import           Data.List                      ( genericLength )
import           Data.Swagger
import           Data.Time
import           Data.Maybe                     ( catMaybes )

data MonitorSwitch
  = MonitorSwitch
    { value :: Bool
    , fixed :: Bool
    }
  deriving (Show, Eq, Generic, ToSchema, ToJSON)

data MonitorWeather
  = MonitorWeather
    { label :: String
    , temperature :: Temperature
    , humidity :: Humidity
    , date :: UTCTime
    } deriving (Show, Eq, Generic, ToSchema, ToJSON)

data Monitor
  = Monitor
    { date :: UTCTime
    , sensorTemperatur :: Maybe Temperature
    , sensorHumidity :: Maybe Humidity
    , sensorTemperaturOutside :: Maybe Temperature
    , sensorHumidityOutside :: Maybe Humidity
    , switchLight1 :: Maybe MonitorSwitch
    , switchLight2 :: Maybe MonitorSwitch
    , weather :: [MonitorWeather]
    , webcamDate :: Maybe UTCTime
    }
  deriving (Show, Eq, Generic, ToSchema, ToJSON)

from :: UTCTime -> [CDB.Status] -> CST.State -> COM.ForecastResult -> Monitor
from date status state forecast = Monitor date
                                          (mean' CDB.temperature)
                                          (mean' CDB.humidity)
                                          (mean' CDB.temperatureOutside)
                                          (mean' CDB.humidityOutside)
                                          (fromSwitch <$> CST.light1 state)
                                          (fromSwitch <$> CST.light2 state)
                                          (fromForecast forecast)
                                          (CST.webcamDate state)
 where
  mean' f = mean . catMaybes $ f <$> status
  fromSwitch (CST.Manual     value) = MonitorSwitch value True
  fromSwitch (CST.Controlled value) = MonitorSwitch value False
  mean [] = Nothing
  mean (head : tail) =
    let stail = take 4 tail
    in  Just $ (head + sum stail) / (1 + genericLength stail)
  fromForecast (COM.ForecastResult _ forecats) =
    mapForecast <$> take 6 forecats
  mapForecast forecast = MonitorWeather
    { label       = renderLabel (COM.weather forecast)
    , temperature = COM.temperature forecast
    , humidity    = COM.humidity forecast
    , date        = COM.date forecast
    }
  renderLabel Nothing = "Unspecified Weather"
  renderLabel (Just (COM.ForecastWeather name descr)) = name <> " - " <> descr
