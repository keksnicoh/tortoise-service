{-# LANGUAGE FlexibleContexts #-}
module Content.Service.TimeSeriesService
  ( TimeSeriesService
  , mkTimeSeriesService
  , GroupedTimeSeriesService
  , mkGroupedTimeSeriesService
  )
where

import qualified Content.Model.TimeSeries   as TimeSeries
import           Control.Monad.Reader       (MonadReader)
import qualified Core.Database.Model.Status as CStatus
import           Data.Maybe                 (fromMaybe)
import qualified Data.Time                  as T
import           OpenEnv                    (Embedded, embedded)

type TimeSeriesService m
  =  Maybe T.UTCTime -- start time, default value: `end` - `defaultPeriod`
  -> Maybe T.UTCTime -- end time, default value: now
  -> m TimeSeries.TimeSeries

type GroupedTimeSeriesService m
  =  Maybe T.UTCTime -- start time, see `TimeSeriesService m`
  -> Maybe T.UTCTime -- end time, see `TimeSeriesService m`
  -> Maybe T.NominalDiffTime
  -> m TimeSeries.TimeSeries

defaultDt, defaultPeriod :: T.NominalDiffTime
defaultDt = 60
defaultPeriod = 24 * 3600

{- |returns a time series of the temperature and humiditiy within a defined
   timeframe (start, end). If end is undefined, then end=now. If start is
   undefined, then start=end -1hour. -}
mkTimeSeriesService
  :: (MonadReader e m, Embedded T.UTCTime e m)
  => CStatus.FetchStatusPeriodRepository m
  -> TimeSeriesService m
mkTimeSeriesService fetchStatusPeriodRepository startOpt endOpt = do
  (start, end) <- case (startOpt, endOpt) of
    (Nothing   , Nothing ) -> periodEnd <$> embedded
    (Just start, Nothing ) -> (,) start <$> embedded
    (Nothing   , Just end) -> return $ periodEnd end
    (Just start, Just end) -> return (start, end)
  TimeSeries.from <$> fetchStatusPeriodRepository (start, end)
  where periodEnd time = (T.addUTCTime (-defaultPeriod) time, time)

-- |using a `TimeSeriesService m` this service returns the resukt convoluted
mkGroupedTimeSeriesService
  :: (Functor m) => TimeSeriesService m -> GroupedTimeSeriesService m
mkGroupedTimeSeriesService timeSeriesService startOpt endOpt dt = do
  let dt'     = fromMaybe defaultDt dt
      grouped = TimeSeries.group dt'
  TimeSeries.groupTimeSeries dt' <$> timeSeriesService startOpt endOpt
