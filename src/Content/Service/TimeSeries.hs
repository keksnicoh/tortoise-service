module Content.Service.TimeSeries
  ( mkTimeSeriesService
  , TimeSeriesService
  )
where

import qualified Data.Time                     as T
import           Core.Database.Model.Status
import           Content.Model.TimeSeries
import           Control.Monad.Reader
import           Dependencies

type TimeSeriesService m = Maybe T.UTCTime -> Maybe T.UTCTime -> m TimeSeries

defaultTimeFrame :: T.NominalDiffTime
defaultTimeFrame = 3600

{- |returns a time series of the temperature and humiditiy within a defined
   timeframe (start, end). If start is undefined, then start=now. If end is
   undefined, then end=start -1hour. -}
mkTimeSeriesService
  :: (MonadIO m, MonadReader e m, HasCurrentTime e)
  => FetchStatusPeriodRepository m
  -> TimeSeriesService m
mkTimeSeriesService fetchStatusPeriodRepository startOpt endOpt = do
  currentTime  <- reader getCurrentTime
  (start, end) <- case (startOpt, endOpt) of
    (Nothing   , Nothing ) -> liftIO $ periodEnd <$> currentTime
    (Just start, Nothing ) -> return $ periodStart start
    (Nothing   , Just end) -> return $ periodEnd end
    (Just start, Just end) -> return (start, end)
  from start <$> fetchStatusPeriodRepository (start, end)
 where
  periodStart time = (time, T.addUTCTime defaultTimeFrame time)
  periodEnd time = (T.addUTCTime (-defaultTimeFrame) time, time)
