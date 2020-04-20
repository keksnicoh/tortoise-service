{-# LANGUAGE FlexibleInstances #-}
module Core.OpenWeatherMap.Env where

import           Network.HTTP.Client            ( Manager )
import           Core.OpenWeatherMap.Model.Forecast
import           Control.Monad.Reader           ( liftIO
                                                , MonadIO
                                                , reader
                                                , MonadReader
                                                )
import qualified Data.ByteString.Lazy.Internal as LBS
import           Network.HTTP.Client
import           Data.Aeson
import           Control.Exception              ( Exception )
import           Network.HTTP.Types.Status      ( Status
                                                , ok200
                                                )
import           Control.Monad.Catch            ( throwM
                                                , MonadThrow
                                                )


data OpenWeatherMapEnv
  = OpenWeatherMapEnv
    { managedHttpLbs :: Request -> IO (Response LBS.ByteString)
    , weatherUrl :: String
    }

class HasOpenWeatherMapEnv a where getOpenWeatherMapEnv :: a -> OpenWeatherMapEnv
instance HasOpenWeatherMapEnv OpenWeatherMapEnv where
  getOpenWeatherMapEnv = id
