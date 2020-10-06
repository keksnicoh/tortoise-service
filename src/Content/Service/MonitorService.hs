{-# LANGUAGE FlexibleContexts #-}
module Content.Service.MonitorService
  ( MonitorService
  , mkMonitorService
  )
where

import           Content.Model.Monitor                   (Monitor, from)
import           Control.Monad.Reader                    (MonadReader)
import           Core.Database.Model.Status              (FetchStatusPeriodRepository)
import           Core.OpenWeatherMap.Repository.Forecast (FetchForecastRepository)
import           Core.State.Repository.State             (GetState)
import           Data.Time                               (UTCTime, addUTCTime)
import           OpenEnv                                 (Embedded, embedded)

type MonitorService m = m Monitor
toStart :: UTCTime -> UTCTime
toStart = addUTCTime (-300)

mkMonitorService
  :: (MonadReader e m, Embedded UTCTime e m)
  => GetState m
  -> FetchStatusPeriodRepository m
  -> FetchForecastRepository m
  -> MonitorService m
mkMonitorService getState fetchStatusPeriodRepository fetchForecastRepository =
  do
    now    <- embedded
    result <- fetchStatusPeriodRepository (period now)
    from now result <$> getState <*> fetchForecastRepository
  where period time = (toStart time, time)
