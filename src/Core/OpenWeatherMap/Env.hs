{-# LANGUAGE FlexibleInstances #-}

module Core.OpenWeatherMap.Env where

import qualified Data.ByteString.Lazy.Internal as LBS
import           Network.HTTP.Client

data OpenWeatherMapEnv
  = OpenWeatherMapEnv
    { managedHttpLbs :: Request -> IO (Response LBS.ByteString)
    , weatherUrl :: String
    }

class HasOpenWeatherMapEnv a where getOpenWeatherMapEnv :: a -> OpenWeatherMapEnv
instance HasOpenWeatherMapEnv OpenWeatherMapEnv where
  getOpenWeatherMapEnv = id
