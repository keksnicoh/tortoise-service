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

createEnvironment :: IO Env
createEnvironment = do
  putStrLn "read environment..."
  tortoiseConnectionStr <- lookupEnvRequired "TORTOISE_SERVICE_PSQL"
  assetsPath <- lookupEnvRequired "TORTOISE_SERVICE_ASSETS_PATH"
  openWeatherMapApi <- lookupEnvRequired "TORTOISE_SERVICE_OPEN_WEATHER_MAP_API"
  applicationMode <- lookupEnvRequired "TORTOISE_SERVICE_APPLICATION_MODE" >>= \case
    "development" -> return Development
    "staging"     -> return Staging
    "production"  -> return Production
    _ ->
      error
        "invalid application mode, valid application modes are: development, staging, production"
  port <- readMaybe <$> lookupEnvRequired "TORTOISE_SERVICE_PORT" >>= \case
    Just port -> return port
    Nothing   -> error "foo"

  putStrLn "initialize state..."
  state <- newIORef initialState

  putStrLn "connect to database..."
  dbConnection <- connectPostgreSQL $ BS.packChars tortoiseConnectionStr

  putStrLn "create OpenWeatherMap TlsManager"
  openWeatherMapTlsManager <- newManager tlsManagerSettings

  return $ Env
    { applicationMode   = applicationMode
    , dbConnection      = dbConnection
    , port              = port
    , currentTime       = T.getCurrentTime
    , randomUUID        = nextRandom
    , state             = state
    , openWeatherMapEnv = OpenWeatherMapEnv
                            { managedHttpLbs =
                              (`httpLbs` openWeatherMapTlsManager)
                            , weatherUrl     = openWeatherMapApi
                            }
    , assetsPath        = assetsPath
    }
 where
  lookupEnvRequired env = lookupEnv env >>= \case
    Just val -> return val
    Nothing  -> error ("missing environment: " <> env)
