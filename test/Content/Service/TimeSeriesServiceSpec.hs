module Content.Service.TimeSeriesServiceSpec where

import Test.Hspec

import Content.Service.TimeSeriesService
import qualified Core.Database.Model.Status as C
import TestUtil
import Content.Model.TimeSeries
import qualified Data.UUID as UUID
import Data.Time
import Control.Monad.Reader (ReaderT(runReaderT))

spec :: Spec
spec = do
  describe "mkTimeSeriesService" $ do
    let
      timeMockNotExpectedToBeInvoke = fail "should not be invoked" :: IO UTCTime
      timeNow = read "2019-02-03 13:37:31"
      timeMock = return timeNow :: IO UTCTime
      records = 
        [ C.Status
          { C.statusId = UUID.nil
          , C.temperature = Just 1
          , C.humidity = Just 1
          , C.temperatureOutside = Nothing
          , C.humidityOutside = Just 5
          , C.created = read "2019-02-03 13:37:45" 
          }
        , C.Status
          { C.statusId = UUID.nil
          , C.temperature = Just 2
          , C.humidity = Just 3
          , C.temperatureOutside = Just 9
          , C.humidityOutside = Just 5
          , C.created = read "2019-02-03 13:37:42"
          }
        ]
    it "should pass a defined period and transform the result" $ do
      let
        start      = read "2019-02-03 13:37:30"
        end        = read "2020-02-03 13:37:50"
        repository = mockSingular (`shouldBe` (start, end)) records
        service    = mkTimeSeriesService repository
        effect     = service (Just start) (Just end)
      runReaderT effect timeMockNotExpectedToBeInvoke >>= flip shouldBe (from records)
    it "should use (now - 24hours, now) for undefined period" $ do
      let
        start      = read "2019-02-02 13:37:31"
        repository = mockSingular (`shouldBe` (start, timeNow)) records
        service    = mkTimeSeriesService repository
        effect     = service Nothing Nothing
      runReaderT effect timeMock >>= flip shouldBe (from records)
    it "should use (start, now) for undefined end" $ do
      let
        start      = read "2019-02-03 11:33:31"
        end        = timeNow
        repository = mockSingular (`shouldBe` (start, end)) records
        service    = mkTimeSeriesService repository
        effect     = service (Just start) Nothing 
      runReaderT effect timeMock >>= flip shouldBe (from records)
    it "should use (end - 24hours, end) for undefined start" $ do
      let
        start      = read "2019-02-02 13:11:22"
        end        = read "2019-02-03 13:11:22"
        repository = mockSingular (`shouldBe` (start, end)) records
        service    = mkTimeSeriesService repository
        effect     = service Nothing (Just end)
      runReaderT effect timeMock >>= flip shouldBe (from records)

  describe "mkGroupedTimeSeriesService" $ do
    let
      timeSeries = TimeSeries
        [ Point (read "2019-02-03 13:11:22") 14, Point (read "2019-02-03 13:10:22") 13, Point (read "2019-02-03 13:05:22") 11 ]
        [ Point (read "2019-02-03 13:05:04") 14, Point (read "2019-02-03 13:02:01") 1 ]
        [ Point (read "2019-02-03 13:11:22") 22, Point (read "2019-02-03 13:05:22") 22, Point (read "2019-02-03 13:00:22") 11 ]
        [ Point (read "2019-02-03 13:22:22") 14, Point (read "2019-02-03 13:10:22") 13, Point (read "2019-02-03 13:00:22") 23 ]
      start        = Just $ read "2019-02-02 13:11:22"
      end          = Just $ read "2019-02-03 13:11:22"
      innerService = mockTwo (`shouldBe` start) (`shouldBe` end) timeSeries
      service      = mkGroupedTimeSeriesService innerService :: GroupedTimeSeriesService IO
    it "should group records using default value of 60" $ do
      let
        effect = service start end Nothing 
      effect >>= flip shouldBe (groupTimeSeries 60 timeSeries)
    it "should group records using defined value of 600" $ do
      let
        effect = service start end (Just 600) 
      effect >>= flip shouldBe (groupTimeSeries 600 timeSeries)
