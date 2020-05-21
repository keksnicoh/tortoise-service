{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bootstrap where

import           Database.PostgreSQL.Simple
import           Env
import           Data.UUID.V4                   ( nextRandom )
import qualified Data.Time                     as T
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
import           Automation.Model.HouseStateConfig
                                                ( HouseStateConfig(..) )
import           Automation.Model.SimpleHandlerConfig
                                                ( SimpleHandlerConfig(..)
                                                , TRange(..)
                                                )
import           Control.Concurrent             ( threadDelay )
import           Control.Monad.Reader           ( MonadIO(liftIO) )

defaultPort :: String
defaultPort = "8081"

-- |creates an environment by reading system environment.
createEnvironment :: MonadIO m => IO (Env m)
createEnvironment = do
  putStrLn "read environment..."
  psqlConnectionString <- envPSQL (e "PSQL")
  assetsPath           <- requiredEnv (e "ASSETS_PATH")
  openWeatherMapApi    <- requiredEnv (e "OPEN_WEATHER_MAP_API")
  applicationMode      <- envApplicationMode (e "APPLICATION_MODE")
  port                 <- envPort (e "PORT")

  fsmEmergencyDelay    <- envReadOpt (e "FSM_EMERGENCY_DELAY") "900"
  fsmSensorDelay       <- envReadOpt (e "FSM_SENSOR_DELAY") "60"
  fsmMinTemperature    <- envReadOpt (e "FSM_MIN_TEMPERATURE") "11"
  fsmMaxTemperature    <- envReadOpt (e "FSM_MAX_TEMPERATURE") "35"
  fsmRetry             <- envReadOpt (e "FSM_RETRY") "5"
  fsmScL1TLow <- TRange <$> envReadOpt (e "FSM_SC_L1_TLOW") "16" <*> envReadOpt
    (e "FSM_SC_L1_THIGH")
    "25"
  fsmScL2TLow <- TRange <$> envReadOpt (e "FSM_SC_L2_TLOW") "20" <*> envReadOpt
    (e "FSM_SC_L2_THIGH")
    "34"
  fsmScLockDuration <- envReadOpt (e "FSM_SC_LOCK_DURATION") "600"

  putStrLn "initialize storages..."
  state                    <- newIORef initialState
  dbConnection             <- connectPostgreSQL psqlConnectionString
  openWeatherMapTlsManager <- newManager tlsManagerSettings

  return
    $ let houseStateConfig = HouseStateConfig
            { delaySensorRead = fsmSensorDelay * 1000000
            , minTemperature  = fsmMinTemperature
            , maxTemperature  = fsmMaxTemperature
            , retrySensorRead = fsmRetry
            , maxStatusAge    = 600
            , emergencyDelay  = threadDelay $ fsmEmergencyDelay * 1000000
            }
          openWeatherMapEnv = OpenWeatherMapEnv
            { managedHttpLbs = (`httpLbs` openWeatherMapTlsManager)
            , weatherUrl     = openWeatherMapApi
            }
      in  Env
            { applicationMode     = applicationMode
            , dbConnection        = dbConnection
            , port                = port
            , currentTime         = liftIO T.getCurrentTime
            , randomUUID          = liftIO nextRandom
            , state               = state
            , openWeatherMapEnv   = openWeatherMapEnv
            , assetsPath          = assetsPath
            , houseStateConfig    = houseStateConfig
            , simpleHandlerConfig = SimpleHandlerConfig
                                      { l1TRange     = fsmScL1TLow
                                      , l2TRange     = fsmScL2TLow
                                      , lockDuration = fromInteger
                                                         fsmScLockDuration
                                      }
            }
 where
  e v = "TORTOISE_SERVICE_" <> v
  requiredEnv env = lookupEnv env >>= \case
    Just val -> return val
    Nothing  -> error ("missing environment: " <> env)
  envPort env = readMaybe . fromMaybe defaultPort <$> lookupEnv env >>= \case
    Just port -> return port
    Nothing   -> error $ errorPort env
  envReadOpt env value =
    readMaybe . fromMaybe value <$> lookupEnv env >>= \case
      Just v  -> return v
      Nothing -> error $ errorInt env
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
  errorInt env = env <> " not parsable"
