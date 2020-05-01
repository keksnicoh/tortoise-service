{-# LANGUAGE OverloadedStrings #-}

module Content.Model.TimeSeriesSpec where

import Content.Model.TimeSeries
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