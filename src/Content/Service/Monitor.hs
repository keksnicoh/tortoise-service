module Content.Service.Monitor where

import           Content.Model.Monitor          ( from, Monitor )
import           Core.State.Repository.State    ( GetState )
import           Core.Database.Model.Status     ( FetchStatusPeriodRepository )
import           Data.Time                      ( addUTCTime
                                                , UTCTime
                                                )
import           Dependencies
import           Control.Monad.Reader           ( liftIO
                                                , MonadIO
                                                , reader
                                                , MonadReader
                                                )
import           Core.OpenWeatherMap.Repository.Forecast

type MonitorService m = m Monitor
toStart :: UTCTime -> UTCTime
toStart = addUTCTime (-300)

mkMonitorService
  :: (MonadIO m, MonadReader e m, HasCurrentTime e)
  => GetState m
  -> FetchStatusPeriodRepository m
  -> FetchForecastRepository m
  -> MonitorService m
mkMonitorService getState fetchStatusPeriodRepository fetchForecastRepository =
  do
    now    <- reader getCurrentTime >>= liftIO
    result <- fetchStatusPeriodRepository (period now)
    from now result <$> getState <*> fetchForecastRepository

   --  >>= \case
   --   [] -> return $ Left "no state available in past 5 minutes"
   --   (x : xs) ->
   --     Right <$> (from now (x :| xs) <$> getState <*> fetchForecastRepository)
   -- return $ MR.from result
  where period time = (toStart time, time)
