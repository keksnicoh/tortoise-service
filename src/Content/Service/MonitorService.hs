module Content.Service.MonitorService
  ( MonitorService
  , mkMonitorService
  )
where

import           Content.Model.Monitor          ( from
                                                , Monitor
                                                )
import           Core.State.Repository.State    ( GetState )
import           Core.Database.Model.Status     ( FetchStatusPeriodRepository )
import           Data.Time                      ( addUTCTime
                                                , UTCTime
                                                )
import           Dependencies
import           Control.Monad.Reader           ( join
                                                , reader
                                                , MonadReader
                                                )
import           Core.OpenWeatherMap.Repository.Forecast

type MonitorService m = m Monitor
toStart :: UTCTime -> UTCTime
toStart = addUTCTime (-300)

mkMonitorService
  :: (Monad m, MonadReader e m, HasCurrentTime e m, HasLogger e m)
  => GetState m
  -> FetchStatusPeriodRepository m
  -> FetchForecastRepository m
  -> MonitorService m
mkMonitorService getState fetchStatusPeriodRepository fetchForecastRepository =
  do
    now    <- join (reader getCurrentTime)
    result <- fetchStatusPeriodRepository (period now)
    from now result <$> getState <*> fetchForecastRepository
  where period time = (toStart time, time)
