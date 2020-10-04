{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module Core.OpenWeatherMap.Repository.Forecast where

import           Core.OpenWeatherMap.Model.Forecast
                                                ( ForecastResult )
import           Core.OpenWeatherMap.Env        ( OpenWeatherMapEnv
                                                  ( weatherUrl
                                                  , managedHttpLbs
                                                  )
                                                )
import           OpenEnv                        ( provide
                                                , Provides
                                                )
import           Control.Monad.Reader           ( liftIO
                                                , MonadIO
                                                , MonadReader
                                                )
import           Network.HTTP.Client            ( Response
                                                  ( responseStatus
                                                  , responseBody
                                                  )
                                                , parseRequest
                                                )
import           Data.Aeson                     ( eitherDecode )
import           Control.Exception              ( Exception )
import           Network.HTTP.Types.Status      ( Status
                                                , ok200
                                                )
import           Control.Monad.Catch            ( throwM
                                                , MonadThrow
                                                )

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
