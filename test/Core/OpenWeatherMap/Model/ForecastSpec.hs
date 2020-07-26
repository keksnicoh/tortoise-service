module Core.OpenWeatherMap.Model.ForecastSpec where

import           Core.OpenWeatherMap.Model.Forecast
import           Test.Hspec
import           Data.Aeson

import qualified Data.ByteString.Lazy          as BSL

expectedResult :: ForecastResult
expectedResult = ForecastResult
  { cod  = "200"
  , list = [ Forecast { date           = read "2019-03-27 18:00:00Z"
                      , temperature    = 288.02
                      , temperatureMin = 287.858
                      , temperatureMax = 288.02
                      , humidity       = 100
                      , weather = Just (ForecastWeather "Clear" "clear sky")
                      , wind           = 10.14
                      }
           , Forecast { date           = read "2019-03-27 19:00:00Z"
                      , temperature    = 288.12
                      , temperatureMin = 288
                      , temperatureMax = 288.12
                      , humidity       = 100
                      , weather = Just (ForecastWeather "Clouds" "few clouds")
                      , wind           = 10.51
                      }
           ]
  }

spec :: Spec
spec = describe "ForecastResult" $
  it "should parse example json properly" $ do
    contents <- BSL.readFile "test/fixtures/core/open-weather-map/forecast0.json"
    eitherDecode contents `shouldBe` Right expectedResult
