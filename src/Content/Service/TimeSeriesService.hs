module Content.Service.TimeSeriesService where

import qualified Core.Database.Model.Status    as CStatus
import qualified Content.Model.TimeSeriesModel as TimeSeriesModel
import qualified Dependencies                  as D
import qualified Data.Time                     as T
import           Control.Monad.Reader
import           Data.Maybe                     ( fromMaybe )

type TimeSeriesService m
  =  Maybe T.UTCTime -- start time, default value: `end` - `defaultPeriod`
  -> Maybe T.UTCTime -- end time, default value: now
  -> m TimeSeriesModel.TimeSeries

type GroupedTimeSeriesService m
  =  Maybe T.UTCTime -- start time, see `TimeSeriesService m`
  -> Maybe T.UTCTime -- end time, see `TimeSeriesService m`
  -> Maybe T.NominalDiffTime
  -> m TimeSeriesModel.TimeSeries

defaultDt, defaultPeriod :: T.NominalDiffTime
defaultDt = 60
defaultPeriod = 24 * 3600

{- |returns a time series of the temperature and humiditiy within a defined
   timeframe (start, end). If end is undefined, then end=now. If start is
   undefined, then start=end -1hour. -}
mkTimeSeriesService
  :: (MonadIO m, MonadReader e m, D.HasCurrentTime e)
  => CStatus.FetchStatusPeriodRepository m
  -> TimeSeriesService m
mkTimeSeriesService fetchStatusPeriodRepository startOpt endOpt = do
  currentTime  <- reader D.getCurrentTime
  (start, end) <- case (startOpt, endOpt) of
    (Nothing   , Nothing ) -> liftIO $ periodEnd <$> currentTime
    (Just start, Nothing ) -> liftIO $ (,) start <$> currentTime
    (Nothing   , Just end) -> return $ periodEnd end
    (Just start, Just end) -> return (start, end)
  TimeSeriesModel.from <$> fetchStatusPeriodRepository (start, end)
  where periodEnd time = (T.addUTCTime (-defaultPeriod) time, time)

-- |using a `TimeSeriesService m` this service returns the resukt convoluted
mkGroupedTimeSeriesService
  :: (Monad m) => TimeSeriesService m -> GroupedTimeSeriesService m
mkGroupedTimeSeriesService timeSeriesService startOpt endOpt dt = do
  timeSeries <- timeSeriesService startOpt endOpt
  let dt'     = fromMaybe defaultDt dt
      grouped = TimeSeriesModel.group dt'
  return (TimeSeriesModel.groupTimeSeries dt' timeSeries)
