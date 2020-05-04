{-# LANGUAGE OverloadedStrings #-}

module Content.Model.TimeSeriesSpec where

import qualified Core.Database.Model.Status as Status
import Content.Model.TimeSeries
import qualified Data.UUID
import GHC.Exts
import Data.Aeson
import Test.Hspec

spec :: Spec
spec = do
  let
    dates =
      [ read "2019-02-03 13:37:42"
      , read "2019-02-03 13:37:43" 
      , read "2019-02-03 13:37:44" 
      , read "2019-02-03 13:37:45" 
      , read "2019-02-03 13:37:46" 
      , read "2019-02-03 13:37:47" 
      , read "2019-02-03 13:37:48" 
      , read "2019-02-03 13:37:49" 
      , read "2019-02-03 13:37:50"
      ]
  describe "TimeSeries#toJSON" $
    it "should serialize to json properly" $
      let
        timeSeries = TimeSeries
          [ Point (head dates) 2, Point (dates !! 1) 3]
          [ Point (dates !! 2) 4, Point (dates !! 3) 5]
          [ Point (dates !! 4) 6, Point (dates !! 5) 7]
          [ Point (dates !! 6) 8, Point (dates !! 7) 9]
        point d v = Object $ fromList [("x", String d), ("y", Number v)]
        expectedValue = Object $ fromList
          [ ("temperature",        Array $ fromList [ point "2019-02-03T13:37:42Z" 2, point "2019-02-03T13:37:43Z" 3 ])
          , ("humidity",           Array $ fromList [ point "2019-02-03T13:37:44Z" 4, point "2019-02-03T13:37:45Z" 5 ])
          , ("temperatureOutside", Array $ fromList [ point "2019-02-03T13:37:46Z" 6, point "2019-02-03T13:37:47Z" 7 ])
          , ("humidityOutside",    Array $ fromList [ point "2019-02-03T13:37:48Z" 8, point "2019-02-03T13:37:49Z" 9 ])
          ]
      in toJSON timeSeries `shouldBe` expectedValue
  describe "from" $ do
    let
      date = read "2021-02-03 13:37:42"
      emptyStatus = Status.Status
        { Status.statusId = Data.UUID.nil
        , Status.temperature = Nothing
        , Status.humidity = Nothing
        , Status.temperatureOutside = Nothing
        , Status.humidityOutside = Nothing
        , Status.created = read "2020-02-03 13:37:42"
        }
    {-it "should create samples of defined points" $ 
      let
        status1 = emptyStatus
          { Status.statusId = Data.UUID.nil
          , Status.temperature = Just 5
          , Status.humidity = Just 7
          , Status.temperatureOutside = Just 7
          , Status.humidityOutside = Just 8
          , Status.created = read "2020-02-03 13:37:42"
          }
        status2 = status1
          { Status.temperature = Nothing
          , Status.humidity = Just 1
          , Status.temperatureOutside = Just 2
          , Status.humidityOutside = Nothing
          , Status.created = read "2020-02-04 13:37:42"
          }
        status3 = status1
          { Status.temperature = Just 4
          , Status.humidity = Nothing
          , Status.temperatureOutside = Nothing
          , Status.humidityOutside = Just 3
          , Status.created = read "2020-02-05 13:37:42"
          }
      in from date 1 [status1, status2, status3] `shouldBe` TimeSeries
        [ Point (read "2020-02-05 13:37:42") 4, Point (read "2020-02-03 13:37:42") 5 ]
        [ Point (read "2020-02-04 13:37:42") 1, Point (read "2020-02-03 13:37:42") 7 ]
        [ Point (read "2020-02-04 13:37:42") 2, Point (read "2020-02-03 13:37:42") 7 ]
        [ Point (read "2020-02-05 13:37:42") 3, Point (read "2020-02-03 13:37:42") 8 ]-}
    it "should work for edge case of empty status list" $
      from date 1 [] `shouldBe` TimeSeries [] [] [] []
    it "should work for edge case of list of empty status" $
      from date 1 [emptyStatus, emptyStatus, emptyStatus] `shouldBe` TimeSeries [] [] [] []
