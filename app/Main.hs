{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Network.Wai.Handler.Warp
import           Database.PostgreSQL.Simple
import           Env
import           Server
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

main :: IO ()
main = do
  putStrLn "read environment..."
  tortoiseConnectionStr <- lookupEnvRequired "TORTOISE_SERVICE_PSQL"
  assetsPath <- lookupEnvRequired "TORTOISE_SERVICE_ASSETS_PATH"
  openWeatherMapApi <- lookupEnvRequired "TORTOISE_SERVICE_OPEN_WEATHER_MAP_API"

  putStrLn "initialize state..."
  state <- newIORef initialState

  putStrLn "connect to database..."
  dbConnection <- connectPostgreSQL $ BS.packChars tortoiseConnectionStr
  -- host='localhost' user='postgres' password='docker' dbname='test'
  putStrLn "create OpenWeatherMap TlsManager"
  openWeatherMapTlsManager <- newManager tlsManagerSettings

  let env = Env
        { dbConnection      = dbConnection
        , port              = 8081
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

  putStrLn "run server..."
  run (port env) (turtleApp env)
 where
  lookupEnvRequired env = lookupEnv env >>= \case
    Just val -> return val
    Nothing  -> error ("missing environment: " <> env)
