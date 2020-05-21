{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Content.Model.MonitorSpec where

import qualified Core.State.Model.State        as CSMState
import qualified Core.Database.Model.Status    as CDMStatus
import qualified Core.OpenWeatherMap.Model.Forecast
                                               as COMForecast
import           Content.Model.Switch
import           Content.Model.Monitor
import           Test.Hspec
import           Data.Aeson
import           GHC.Exts
import qualified Data.UUID

spec :: Spec
spec = do
  describe "Monitor#ToJSON" $ do
    it "should serialize to JSON properly" $ do
      let
        monitor = Monitor
          { date = read "2019-03-04 13:37:42"
          , sensorTemperatur = Just 2
          , sensorHumidity = Just 4
          , sensorTemperaturOutside = Just 7
          , sensorHumidityOutside = Just 3
          , switchLight1 = Just (Switch False False)
          , switchLight2 = Just (Switch True False)
          , weather =
            [ MonitorWeather "foo" 12 43 (read "2019-03-04 14:37:42") ]
          , webcamDate = Just (read "2020-03-04 13:37:42")
          , houseState = "x"
          }
        expectedValue = Object $ fromList
          [ ("date", String "2019-03-04T13:37:42Z")
          , ("sensorTemperatur", Number 2)
          , ("sensorHumidity", Number 4)
          , ("sensorTemperaturOutside", Number 7)
          , ("sensorHumidityOutside", Number 3)
          , ("switchLight1", Object $ fromList
            [ ("value", Bool False)
            , ("controlled", Bool False)
            ])
          , ("switchLight2", Object $ fromList
            [ ("value", Bool True)
            , ("controlled", Bool False)
            ])
          , ("weather", Array $ fromList [
            Object $ fromList
              [ ("label", String "foo")
              , ("temperature", Number 12)
              , ("humidity", Number 43)
              , ("date", String "2019-03-04T14:37:42Z")
              ]
            ])
          , ("webcamDate", String "2020-03-04T13:37:42Z")
          , ("houseState", String "x")
          ]
      toJSON monitor `shouldBe` expectedValue
    it "should serialize to JSON properly - minimal" $ do
      let
        monitor = Monitor
          { date = read "2019-03-04 13:37:42"
          , sensorTemperatur = Nothing
          , sensorHumidity = Nothing
          , sensorTemperaturOutside = Nothing
          , sensorHumidityOutside = Nothing
          , switchLight1 = Nothing
          , switchLight2 = Nothing
          , weather = []
          , webcamDate = Nothing
          , houseState = "x"
          }
        expectedValue = Object $ fromList
          [ ("date", String "2019-03-04T13:37:42Z")
          , ("sensorTemperatur", Null)
          , ("sensorHumidity", Null)
          , ("sensorTemperaturOutside", Null)
          , ("sensorHumidityOutside", Null)
          , ("switchLight1", Null)
          , ("switchLight2", Null)
          , ("weather", Array $ fromList [])
          , ("webcamDate", Null)
          , ("houseState", "x")
          ]
      toJSON monitor `shouldBe` expectedValue

  describe "from" $ do
    let 
      dates =
        [ read "2019-03-04 13:37:42"
        , read "2019-03-04 13:37:43"
        , read "2019-03-04 13:37:44"
        , read "2019-03-04 13:37:45"
        ]
      emptyState = CSMState.State
        { light1 = Nothing
        , light2 = Nothing
        , webcamDate = Nothing
        , webcamRequest = Nothing
        , houseMonitor = CSMState.MonitorOff
        , controlLockDate1 = Nothing
        , controlLockDate2 = Nothing
        }
      emptyForecast =  COMForecast.ForecastResult
        { cod = "200"
        , list = []
        }
      minimalMonitor = Monitor
        { date = dates !! 1
        , sensorTemperatur = Nothing
        , sensorHumidity = Nothing
        , sensorTemperaturOutside = Nothing
        , sensorHumidityOutside = Nothing
        , switchLight1 = Nothing
        , switchLight2 = Nothing
        , weather = []
        , webcamDate = Nothing
        , houseState = "MonitorOff"
        }
      minimalStatus = CDMStatus.Status
        { CDMStatus.statusId = Data.UUID.nil
        , CDMStatus.temperature = Nothing
        , CDMStatus.humidity = Nothing
        , CDMStatus.temperatureOutside = Nothing
        , CDMStatus.humidityOutside = Nothing
        , CDMStatus.created = head dates
        }
    it "should produce correct Monitor data" $ do
      let
        status1 = minimalStatus
          { CDMStatus.statusId = Data.UUID.nil
          , CDMStatus.temperature = Just 5
          , CDMStatus.humidity = Just 10
          , CDMStatus.temperatureOutside = Just 7
          , CDMStatus.humidityOutside = Nothing
          , CDMStatus.created = head dates
          }
        status2 = status1
          { CDMStatus.temperature = Nothing
          , CDMStatus.humidity = Just 12
          , CDMStatus.temperatureOutside = Just 7
          , CDMStatus.humidityOutside = Just 8
          }
        status3 = status1
          { CDMStatus.temperature = Nothing
          , CDMStatus.humidity = Nothing
          , CDMStatus.temperatureOutside = Just 7
          , CDMStatus.humidityOutside = Just 16
          }
        status4 = status1
          { CDMStatus.temperature = Nothing
          , CDMStatus.humidity = Just 14
          , CDMStatus.temperatureOutside = Just 7
          , CDMStatus.humidityOutside = Nothing
          }
        state = emptyState
          { CSMState.light1 = Just (CSMState.Manual True)
          , CSMState.light2 = Just (CSMState.Controlled False)
          , CSMState.webcamDate = Just (dates !! 2)
          }
        forecast = COMForecast.Forecast
          { COMForecast.date = dates !! 3
          , COMForecast.temperature = 273.15 + 2
          , COMForecast.temperatureMin = 273.15 + 6
          , COMForecast.temperatureMax = 273.15 + 8
          , COMForecast.humidity = 7
          , COMForecast.weather = Just (COMForecast.ForecastWeather "sun" "foo")
          , COMForecast.wind = 13.37
          }
        forecastResult = emptyForecast
          { COMForecast.cod = "200"
          , COMForecast.list = [forecast]
          }
      from (dates !! 1) [status1, status2, status3, status4] state forecastResult `shouldBe` minimalMonitor
        { date = dates !! 1
        , sensorTemperatur = Just 5
        , sensorHumidity = Just 12
        , sensorTemperaturOutside = Just 7
        , sensorHumidityOutside = Just 12
        , switchLight1 = Just (Switch True False)
        , switchLight2 = Just (Switch False True)
        , weather = [ MonitorWeather "sun - foo" 2 7 (dates !! 3)]
        , webcamDate = Just (dates !! 2)
        }
    it "should work with minimal data and undefined sensor data" $ do
      from (dates !! 1) [minimalStatus] emptyState emptyForecast `shouldBe` minimalMonitor
    it "should work with minimal data and an empty status list" $ do 
      from (dates !! 1) [] emptyState emptyForecast `shouldBe` minimalMonitor
    it "should render a default weather string" $ do 
      let
        forecast = COMForecast.Forecast
          { COMForecast.date = dates !! 3
          , COMForecast.temperature = 273.15 + 2
          , COMForecast.temperatureMin = 273.15 + 6
          , COMForecast.temperatureMax = 273.15 + 8
          , COMForecast.humidity = 7
          , COMForecast.weather = Nothing
          , COMForecast.wind = 13.37
          }
        forecastResult = emptyForecast
          { COMForecast.cod = "200"
          , COMForecast.list = [forecast]
          }
      from (dates !! 1) [] emptyState forecastResult `shouldBe` Monitor
        { date = dates !! 1
        , sensorTemperatur = Nothing
        , sensorHumidity = Nothing
        , sensorTemperaturOutside = Nothing
        , sensorHumidityOutside = Nothing
        , switchLight1 = Nothing
        , switchLight2 = Nothing
        , weather = [ MonitorWeather "unspecified weather" 2 7 (dates !! 3)]
        , webcamDate = Nothing
        , houseState = "MonitorOff"
        }
    it "should take 6 weather items" $ do
      let
        forecast = COMForecast.Forecast
          { COMForecast.date = dates !! 3
          , COMForecast.temperature = 273.15 + 2
          , COMForecast.temperatureMin = 273.15 + 6
          , COMForecast.temperatureMax = 273.15 + 8
          , COMForecast.humidity = 7
          , COMForecast.weather = Nothing
          , COMForecast.wind = 13.37
          }
        forecastResult = emptyForecast
          { COMForecast.cod = "200"
          , COMForecast.list =
            [ forecast { COMForecast.temperature = 273.15 + 17 }
            , forecast { COMForecast.temperature = 273.15 + 18 }
            , forecast { COMForecast.temperature = 273.15 + 19 }
            , forecast { COMForecast.temperature = 273.15 + 20 }
            , forecast { COMForecast.temperature = 273.15 + 21 }
            , forecast { COMForecast.temperature = 273.15 + 22 }
            , forecast { COMForecast.temperature = 273.15 + 23 }
            ]
          }
      from (dates !! 1) [] emptyState forecastResult `shouldBe` Monitor
        { date = dates !! 1
        , sensorTemperatur = Nothing
        , sensorHumidity = Nothing
        , sensorTemperaturOutside = Nothing
        , sensorHumidityOutside = Nothing
        , switchLight1 = Nothing
        , switchLight2 = Nothing
        , weather = 
          [ MonitorWeather "unspecified weather" 17 7 (dates !! 3)
          , MonitorWeather "unspecified weather" 18 7 (dates !! 3)
          , MonitorWeather "unspecified weather" 19 7 (dates !! 3)
          , MonitorWeather "unspecified weather" 20 7 (dates !! 3)
          , MonitorWeather "unspecified weather" 21 7 (dates !! 3)
          , MonitorWeather "unspecified weather" 22 7 (dates !! 3)
          ]
        , webcamDate = Nothing
        , houseState = "MonitorOff"
        }
