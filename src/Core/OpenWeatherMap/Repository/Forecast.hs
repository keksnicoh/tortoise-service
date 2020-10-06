{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE FlexibleContexts #-}

module Core.OpenWeatherMap.Repository.Forecast where

import           Control.Exception                  (Exception)
import           Control.Monad.Catch                (MonadThrow, throwM)
import           Control.Monad.Reader               (MonadIO, MonadReader,
                                                     liftIO)
import           Core.OpenWeatherMap.Env            (OpenWeatherMapEnv (managedHttpLbs, weatherUrl))
import           Core.OpenWeatherMap.Model.Forecast (ForecastResult)
import           Data.Aeson                         (eitherDecode)
import           Network.HTTP.Client                (Response (responseBody, responseStatus),
                                                     parseRequest)
import           Network.HTTP.Types.Status          (Status, ok200)
import           OpenEnv                            (Provides, provide)

type FetchForecastRepository m = m ForecastResult

data ForecastRepositoryException
  = InvalidResponseBodyException String
  | UnexpectedHttpStatusException Status
  deriving (Eq, Show, Exception)

forecastRepository
  :: (MonadIO m, MonadThrow m, MonadReader e m, Provides OpenWeatherMapEnv e)
  => FetchForecastRepository m
forecastRepository = do
  env <- provide
  liftIO (parseRequest (weatherUrl env) >>= managedHttpLbs env) >>= handle
 where
  handle response = case responseStatus response of
    s | s == ok200 -> case eitherDecode (responseBody response) of
      Right forecaseResult -> return forecaseResult
      Left  err            -> throwM $ InvalidResponseBodyException err
    s -> throwM $ UnexpectedHttpStatusException s
