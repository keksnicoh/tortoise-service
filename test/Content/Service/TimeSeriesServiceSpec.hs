{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Content.Service.TimeSeriesServiceSpec where

import           Test.Hspec

import           Content.Service.TimeSeriesService
import qualified Core.Database.Model.Status    as C
import           TestUtil
import           Content.Model.TimeSeries
import qualified Data.UUID                     as UUID
import           Data.Time
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import           Dependencies
import           Control.Monad.Identity         ( runIdentity
                                                , Identity
                                                )

type RT = ReaderT (Identity UTCTime) Identity
instance HasCurrentTime (Identity UTCTime) RT where
  getCurrentTime = return . runIdentity

spec :: Spec
spec = do
  describe "mkTimeSeriesService" $ do
    let timeMockNotExpectedToBeInvoke = undefined 
        timeNow                       = read "2019-02-03 13:37:31" 
        timeMock                      = return timeNow
        records =
          [ C.Status { C.statusId           = UUID.nil
                     , C.temperature        = Just 1
                     , C.humidity           = Just 1
                     , C.temperatureOutside = Nothing
                     , C.humidityOutside    = Just 5
                     , C.created            = read "2019-02-03 13:37:45"
                     }
          , C.Status { C.statusId           = UUID.nil
                     , C.temperature        = Just 2
                     , C.humidity           = Just 3
                     , C.temperatureOutside = Just 9
                     , C.humidityOutside    = Just 5
                     , C.created            = read "2019-02-03 13:37:42"
                     }
          ]
        mockRepository expected result = mock
         where
          mock :: C.FetchStatusPeriodRepository RT
          mock arg | arg == expected = return result
                   | otherwise       = error $ "unexpected invocation: " ++ show arg
    it "should pass a defined period and transform the result" $ do
      let start      = read "2019-02-03 13:37:30"
          end        = read "2020-02-03 13:37:50"
          repository = mockRepository (start, end) records
          service    = mkTimeSeriesService repository
          effect     = service (Just start) (Just end)
          result     = runReaderT effect timeMockNotExpectedToBeInvoke
      runIdentity result `shouldBe` from records
    it "should use (now - 24hours, now) for undefined period" $ do
      let start      = read "2019-02-02 13:37:31"
          repository = mockRepository (start, timeNow) records
          service    = mkTimeSeriesService repository
          effect     = service Nothing Nothing
          result     = runReaderT effect timeMock
      runIdentity result `shouldBe` from records
    it "should use (start, now) for undefined end" $ do
      let start      = read "2019-02-03 11:33:31"
          end        = timeNow
          repository = mockRepository (start, timeNow) records
          service    = mkTimeSeriesService repository
          effect     = service (Just start) Nothing
          result     = runReaderT effect timeMock
      runIdentity result `shouldBe` from records
    it "should use (end - 24hours, end) for undefined start" $ do
      let start      = read "2019-02-02 13:11:22"
          end        = read "2019-02-03 13:11:22"
          repository = mockRepository (start, end) records
          service    = mkTimeSeriesService repository
          effect     = service Nothing (Just end)
          result     = runReaderT effect timeMock
      runIdentity result `shouldBe` from records

  describe "mkGroupedTimeSeriesService" $ do
    let timeSeries = TimeSeries
          [ Point (read "2019-02-03 13:11:22") 14
          , Point (read "2019-02-03 13:10:22") 13
          , Point (read "2019-02-03 13:05:22") 11
          ]
          [ Point (read "2019-02-03 13:05:04") 14
          , Point (read "2019-02-03 13:02:01") 1
          ]
          [ Point (read "2019-02-03 13:11:22") 22
          , Point (read "2019-02-03 13:05:22") 22
          , Point (read "2019-02-03 13:00:22") 11
          ]
          [ Point (read "2019-02-03 13:22:22") 14
          , Point (read "2019-02-03 13:10:22") 13
          , Point (read "2019-02-03 13:00:22") 23
          ]
        start        = Just $ read "2019-02-02 13:11:22"
        end          = Just $ read "2019-02-03 13:11:22"
        innerService = mockTwo (`shouldBe` start) (`shouldBe` end) timeSeries
        service =
          mkGroupedTimeSeriesService innerService :: GroupedTimeSeriesService IO
    it "should group records using default value of 60" $ do
      let effect = service start end Nothing
      effect >>= flip shouldBe (groupTimeSeries 60 timeSeries)
    it "should group records using defined value of 600" $ do
      let effect = service start end (Just 600)
      effect >>= flip shouldBe (groupTimeSeries 600 timeSeries)
