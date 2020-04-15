module Content.Service.TimeSeries where

import qualified Data.Time                     as T
import           Core.Database.Model.Status
import           Content.Model.TimeSeries
import           Control.Monad.Reader
import           Dependencies
import           Data.Maybe

type TimeSeriesService m = Maybe T.UTCTime -> Maybe T.UTCTime -> m TimeSeries

defaultTimeFrame :: T.NominalDiffTime
defaultTimeFrame = 3600

mkTimeSeriesService
  :: (MonadIO m, MonadReader e m, HasCurrentTime e)
  => FetchStatusPeriodRepository m
  -> TimeSeriesService m
mkTimeSeriesService fetchStatusPeriodRepository startOpt endOpt = do
  currentTime <- reader getCurrentTime
  start       <- liftIO $ case startOpt of
    Just start -> return start
    Nothing    -> T.addUTCTime (-defaultTimeFrame) <$> currentTime
  let end = fromMaybe (T.addUTCTime defaultTimeFrame start) endOpt
  from start <$> fetchStatusPeriodRepository (start, end)
