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
import System.Environment
import Data.ByteString.Internal as BS
--derk =
--  "https://samples.openweathermap.org/data/2.5/forecast/hourly?lat=54&lon=10&appid=e46215c483de286ca6a589305c95a42e"
--53°33'18.7"N 9°54'51.5"E
--data ForecaseRepositoryException = InvalidJsonBody
-- https://samples.openweathermap.org/data/2.5/forecast/hourly?lat=54&lon=10&appid=e46215c483de286ca6a589305c95a42e
main :: IO ()
main = do
  putStrLn "read environment..."
  tortoiseConnectionStr <- lookupEnvRequired "TORTOISE_SERVICE_PSQL"
  assetsPath <- lookupEnvRequired "TORTOISE_SERVICE_ASSETS_PATH"
  
  putStrLn "initialize state..."
  state <- newIORef initialState

  putStrLn "connect to database..."
  dbConnection <- connectPostgreSQL $ BS.packChars tortoiseConnectionStr
  -- host='localhost' user='postgres' password='docker' dbname='test'
  putStrLn "create OpenWeatherMap TlsManager"
  openWeatherMapTlsManager <- newManager tlsManagerSettings

  let
    env = Env
      { dbConnection      = dbConnection
      , port              = 8081
      , currentTime       = T.getCurrentTime
      , randomUUID        = nextRandom
      , state             = state
      , openWeatherMapEnv =
        OpenWeatherMapEnv
          { managedHttpLbs = (`httpLbs` openWeatherMapTlsManager)
          , weatherUrl     =
            "https://api.openweathermap.org/data/2.5/forecast?appid=b24937e45bc0bb6231615290f996a710&lat=54&lon=10"
          }
      , assetsPath = assetsPath
      }

  putStrLn "run server..."
  run (port env) (turtleApp env)
 where
  lookupEnvRequired env = lookupEnv env >>= \case
    Just val -> return val
    Nothing -> error ("missing environment: " <> env)
