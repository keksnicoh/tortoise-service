{-# LANGUAGE DeriveAnyClass #-}
module Core.OpenWeatherMap.Repository.Forecast where

import           Core.OpenWeatherMap.Model.Forecast
import           Core.OpenWeatherMap.Env
import           Control.Monad.Reader           ( liftIO
                                                , MonadIO
                                                , reader
                                                , MonadReader
                                                )
import           Network.HTTP.Client
import           Data.Aeson
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
  :: (MonadIO m, MonadThrow m, MonadReader e m, HasOpenWeatherMapEnv e)
  => FetchForecastRepository m
forecastRepository = do
  env <- reader getOpenWeatherMapEnv
  liftIO (parseRequest (weatherUrl env) >>= managedHttpLbs env) >>= handle
 where
  handle response = case responseStatus response of
    s | s == ok200 -> case eitherDecode (responseBody response) of
      Right forecaseResult -> return forecaseResult
      Left  err            -> throwM $ InvalidResponseBodyException err
    s -> throwM $ UnexpectedHttpStatusException s
