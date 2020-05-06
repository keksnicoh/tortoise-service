{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bootstrap where

import           Database.PostgreSQL.Simple
import           Env
import           Data.UUID.V4                   ( nextRandom )
import qualified Data.Time                     as T
                                                ( getCurrentTime )
import           GHC.IORef                      ( newIORef )
import           Core.State.Model.State         ( initialState )
import           Core.OpenWeatherMap.Env
import           Network.HTTP.Client            ( httpLbs
                                                , newManager
                                                )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           System.Environment
import           Data.ByteString.Internal      as BS
import           Text.Read                      ( readMaybe )
import           Data.Maybe                     ( fromMaybe )

defaultPort :: String
defaultPort = "8081"

-- |creates an environment by reading system environment.
createEnvironment :: IO Env
createEnvironment = do
  putStrLn "read environment..."
  psqlConnectionString <- envPSQL (e "PSQL")
  assetsPath           <- requiredEnv (e "ASSETS_PATH")
  openWeatherMapApi    <- requiredEnv (e "OPEN_WEATHER_MAP_API")
  applicationMode      <- envApplicationMode (e "APPLICATION_MODE")
  port                 <- envPort (e "PORT")

  putStrLn "initialize storages..."
  state                    <- newIORef initialState
  dbConnection             <- connectPostgreSQL psqlConnectionString
  openWeatherMapTlsManager <- newManager tlsManagerSettings

  return
    $ let openWeatherMapEnv = OpenWeatherMapEnv
            { managedHttpLbs = (`httpLbs` openWeatherMapTlsManager)
            , weatherUrl     = openWeatherMapApi
            }
      in  Env { applicationMode   = applicationMode
              , dbConnection      = dbConnection
              , port              = port
              , currentTime       = T.getCurrentTime
              , randomUUID        = nextRandom
              , state             = state
              , openWeatherMapEnv = openWeatherMapEnv
              , assetsPath        = assetsPath
              }
 where
  e v = "TORTOISE_SERVICE_" <> v
  requiredEnv env = lookupEnv env >>= \case
    Just val -> return val
    Nothing  -> error ("missing environment: " <> env)
  envPort env = readMaybe . fromMaybe defaultPort <$> lookupEnv env >>= \case
    Just port -> return port
    Nothing   -> error $ errorPort env
  envApplicationMode env = requiredEnv env >>= \case
    "development" -> return Development
    "staging"     -> return Staging
    "production"  -> return Production
    _             -> error $ errorApplicationMode env
  envPSQL env = BS.packChars <$> requiredEnv env
  errorApplicationMode env =
    env
      <> " not parsable, valid application modes are: "
      <> "development, staging, production"
  errorPort env = env <> " not parsable"
