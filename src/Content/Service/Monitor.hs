{-# LANGUAGE LambdaCase #-}

module Content.Service.Monitor where

import           Content.Model.Monitor          ( from )
import qualified Content.Model.MonitorResult   as MR
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
import           Data.List.NonEmpty             ( NonEmpty((:|)) )


type MonitorService m = m MR.MonitorResult
toStart :: UTCTime -> UTCTime
toStart = addUTCTime (-120)

mkMonitorService
  :: (MonadIO m, MonadReader e m, HasCurrentTime e)
  => GetState m
  -> FetchStatusPeriodRepository m
  -> MonitorService m
mkMonitorService getState fetchStatusPeriodRepository = do
  now    <- reader getCurrentTime >>= liftIO
  result <- fetchStatusPeriodRepository (period now) >>= \case
    []       -> return $ Left "no state available in past 30 minutes"
    (x : xs) -> Right . from (x :| xs) <$> getState
  return $ MR.from result
  where period time = (toStart time, time)
