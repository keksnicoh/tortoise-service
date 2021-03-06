{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.OpenWeatherMap.Model.Forecast
  ( Forecast(..)
  , ForecastResult(..)
  , ForecastWeather(..)
  )
where

import           Core.Internal (Humidity, Temperature)
import           Data.Aeson    (FromJSON (parseJSON), Value (Object), withArray,
                                withObject, (.:))
import           Data.Time     (UTCTime, localTimeToUTC, utc)
import           Data.Vector   ((!?))
import           GHC.Generics  (Generic)

data ForecastResult
  = ForecastResult
    { cod  :: String
    , list :: [Forecast]
    }
  deriving (Show, Eq, Generic, FromJSON)

data ForecastWeather
  = ForecastWeather
    { name        :: String
    , description :: String }
  deriving (Show, Eq)

data Forecast
  = Forecast
    { date           :: UTCTime
    , temperature    :: Temperature
    , temperatureMin :: Temperature
    , temperatureMax :: Temperature
    , humidity       :: Humidity
    , weather        :: Maybe ForecastWeather
    , wind           :: Float
    }
  deriving (Show, Eq)

instance FromJSON Forecast where
  parseJSON = withObject "Forecast" $ \v ->
    Forecast
      <$> (localTimeToUTC utc <$> v .: "dt_txt")
      <*> (v .: "main" \\ "temp")
      <*> (v .: "main" \\ "temp_min")
      <*> (v .: "main" \\ "temp_max")
      <*> (v .: "main" \\ "humidity")
      <*> (v .: "weather" >>= withArray
            "weather"
            (\v -> case v !? 0 of
              Just (Object o) ->
                Just
                  <$> (ForecastWeather <$> o .: "main" <*> o .: "description")
              _ -> return Nothing
            )
          )
      <*> (v .: "wind" \\ "speed")
    where (\\) o key = o >>= flip (.:) key

