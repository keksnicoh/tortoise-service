module Content.Service.TimeSeriesSpec where

import Test.Hspec

import Content.Service.TimeSeries
import qualified Core.Database.Model.Status as C
import TestUtil
import Content.Model.TimeSeries
import qualified Data.UUID as UUID
import Data.Time
import Control.Monad.Reader (ReaderT(runReaderT))

spec :: Spec
spec =
  describe "mkTimeSeriesService" $ do
    let
      timeMockNotExpectedToBeInvoke = fail "should not be invoked" :: IO UTCTime
      timeNow                       = read "2019-02-03 13:37:31"
      timeMock                      = return timeNow :: IO UTCTime
      records = 
        [ C.Status { C.statusId = UUID.nil, C.temperature = 1, C.humidity = 1, C.created = read "2019-02-03 13:37:45" }
        , C.Status { C.statusId = UUID.nil, C.temperature = 2, C.humidity = 3, C.created = read "2019-02-03 13:37:42" }
        ]
    it "should pass a defined period and transform the result" $ do
      let
        dummyTime  = fail "should not be invoked" :: IO UTCTime
        start      = read "2019-02-03 13:37:30"
        end        = read "2020-02-03 13:37:50"
        repository = mockSingular (`shouldBe` (start, end)) records
        service    = mkTimeSeriesService repository
      result <- runReaderT (service (Just start) (Just end)) timeMockNotExpectedToBeInvoke
      result `shouldBe` from start records
    it "use (now - 1hour, now) for undefined period" $ do
      let
        start      = read "2019-02-03 12:37:31"
        repository = mockSingular (`shouldBe` (start, timeNow)) records
        service    = mkTimeSeriesService repository
      result <- runReaderT (service Nothing Nothing) timeMock
      result `shouldBe` from start records
    it "use (start, start + 1hour) for undefined end" $ do
      let
        start      = read "2019-02-03 11:33:31"
        end        = read "2019-02-03 12:33:31"
        repository = mockSingular (`shouldBe` (start, end)) records
        service    = mkTimeSeriesService repository
      result <- runReaderT (service (Just start) Nothing) timeMock
      result `shouldBe` from start records
    it "use (end - 1hour, end) for undefined start" $ do
      let
        start      = read "2019-02-03 12:11:22"
        end        = read "2019-02-03 13:11:22"
        repository = mockSingular (`shouldBe` (start, end)) records
        service    = mkTimeSeriesService repository
      result <- runReaderT (service Nothing (Just end)) timeMock
      result `shouldBe` from start records
