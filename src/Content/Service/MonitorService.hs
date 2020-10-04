{-# LANGUAGE FlexibleContexts #-}
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
import           OpenEnv                        ( embedded
                                                , Embedded
                                                )
import           Control.Monad.Reader           ( MonadReader )
import           Core.OpenWeatherMap.Repository.Forecast
                                                ( FetchForecastRepository )

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
