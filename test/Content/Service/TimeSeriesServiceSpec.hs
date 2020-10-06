{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Content.Service.TimeSeriesServiceSpec where

import           Content.Model.TimeSeries          (Point (Point),
                                                    TimeSeries (TimeSeries),
                                                    from, groupTimeSeries)
import           Content.Service.TimeSeriesService (GroupedTimeSeriesService,
                                                    mkGroupedTimeSeriesService,
                                                    mkTimeSeriesService)
import           Control.Monad.Identity            (Identity, runIdentity)
import           Control.Monad.Reader              (ReaderT (runReaderT))
import qualified Core.Database.Model.Status        as C
import           Data.Time                         (UTCTime)
import qualified Data.UUID                         as UUID
import           OpenEnv                           (nil, ( #: ))
import           Test.Hspec                        (Spec, describe, it,
                                                    shouldBe)
import           TestUtil                          (mockTwo)

spec :: Spec
spec = do
  describe "mkTimeSeriesService" $
    do
      let timeNow = read @UTCTime "2019-02-03 13:37:31Z"
          timeMock = return @Identity timeNow
          records =
            [ C.Status
                { C.statusId = UUID.nil,
                  C.temperature = Just 1,
                  C.humidity = Just 1,
                  C.temperatureOutside = Nothing,
                  C.humidityOutside = Just 5,
                  C.created = read "2019-02-03 13:37:45Z"
                },
              C.Status
                { C.statusId = UUID.nil,
                  C.temperature = Just 2,
                  C.humidity = Just 3,
                  C.temperatureOutside = Just 9,
                  C.humidityOutside = Just 5,
                  C.created = read "2019-02-03 13:37:42Z"
                }
            ]
          mockRepository expected result = mock
            where
              mock arg
                | arg == expected = return result
                | otherwise = error $ "unexpected invocation: " ++ show arg
      it "should pass a defined period and transform the result" $
        do
          let start = read "2019-02-03 13:37:30Z"
              end = read "2020-02-03 13:37:50Z"
              repository = mockRepository (start, end) records
              service = mkTimeSeriesService repository
              effect = service (Just start) (Just end)
              result =
                runReaderT effect (return @Identity @UTCTime undefined #: nil)
          runIdentity result `shouldBe` from records
      it "should use (now - 24hours, now) for undefined period" $
        do
          let start = read "2019-02-02 13:37:31Z"
              repository = mockRepository (start, timeNow) records
              service = mkTimeSeriesService repository
              effect = service Nothing Nothing
              result = runReaderT effect (timeMock #: nil)
          runIdentity result `shouldBe` from records
      it "should use (start, now) for undefined end" $
        do
          let start = read "2019-02-03 11:33:31Z"
              end = timeNow
              repository = mockRepository (start, timeNow) records
              service = mkTimeSeriesService repository
              effect = service (Just start) Nothing
              result = runReaderT effect (timeMock #: nil)
          runIdentity result `shouldBe` from records
      it "should use (end - 24hours, end) for undefined start" $
        do
          let start = read "2019-02-02 13:11:22Z"
              end = read "2019-02-03 13:11:22Z"
              repository = mockRepository (start, end) records
              service = mkTimeSeriesService repository
              effect = service Nothing (Just end)
              result = runReaderT effect (timeMock #: nil)
          runIdentity result `shouldBe` from records
  describe "mkGroupedTimeSeriesService" $
    do
      let timeSeries =
            TimeSeries
              [ Point (read "2019-02-03 13:11:22Z") 14,
                Point (read "2019-02-03 13:10:22Z") 13,
                Point (read "2019-02-03 13:05:22Z") 11
              ]
              [ Point (read "2019-02-03 13:05:04Z") 14,
                Point (read "2019-02-03 13:02:01Z") 1
              ]
              [ Point (read "2019-02-03 13:11:22Z") 22,
                Point (read "2019-02-03 13:05:22Z") 22,
                Point (read "2019-02-03 13:00:22Z") 11
              ]
              [ Point (read "2019-02-03 13:22:22Z") 14,
                Point (read "2019-02-03 13:10:22Z") 13,
                Point (read "2019-02-03 13:00:22Z") 23
              ]
          start = Just $ read "2019-02-02 13:11:22Z"
          end = Just $ read "2019-02-03 13:11:22Z"
          innerService = mockTwo (`shouldBe` start) (`shouldBe` end) timeSeries
          service =
            mkGroupedTimeSeriesService innerService ::
              GroupedTimeSeriesService IO
      it "should group records using default value of 60" $
        do
          let effect = service start end Nothing
          effect >>= flip shouldBe (groupTimeSeries 60 timeSeries)
      it "should group records using defined value of 600" $
        do
          let effect = service start end (Just 600)
          effect >>= flip shouldBe (groupTimeSeries 600 timeSeries)
