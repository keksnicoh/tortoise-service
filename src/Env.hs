{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Env
  ( Env(..)
  , ApplicationMode(..)
  , RT
  )
where

import           Database.PostgreSQL.Simple
import           Core.Database.Env
import qualified Data.Time                     as T
import           Data.UUID                      ( UUID )
import           Dependencies
import           Core.State.Model.State
import           Core.State.Env
import           Core.OpenWeatherMap.Env
import           GHC.IORef                      ( IORef )
import           Control.Monad.Reader           ( ReaderT )
import           Servant
import qualified Automation.Model.HouseStateConfig
                                               as AMHouseStateConfig
import qualified Automation.Model.SimpleHandlerConfig
                                               as AMSimpleHandlerConfig
data ApplicationMode
  = Development
  | Staging
  | Production
  deriving (Eq, Show)

type RT m = ReaderT (Env m) m

data Env m
  = Env
    { applicationMode :: ApplicationMode
    , dbConnection :: Connection
    , port :: Int
    , currentTime :: RT m T.UTCTime
    , randomUUID :: RT m UUID
    , state :: IORef State
    , openWeatherMapEnv :: OpenWeatherMapEnv
    , assetsPath :: FilePath
    , houseStateConfig :: AMHouseStateConfig.HouseStateConfig
    , simpleHandlerConfig :: AMSimpleHandlerConfig.SimpleHandlerConfig
    , logger :: String -> RT m ()
    }

instance HasDbConnection (Env m) where
  getDbConnection = dbConnection

instance HasCurrentTime (Env Handler) (RT Handler)  where
  getCurrentTime = currentTime

instance HasRandomUUID (Env Handler) (RT Handler) where
  getRandomUUID = randomUUID

instance HasState (Env m) where
  getState = state

instance HasOpenWeatherMapEnv (Env m) where
  getOpenWeatherMapEnv = openWeatherMapEnv

instance HasAssetsPath (Env m) where
  getAssetsPath = assetsPath

instance AMHouseStateConfig.HasHouseStateConfig (Env m) where
  getHouseStateConfig = houseStateConfig

instance AMSimpleHandlerConfig.HasSimpleHandlerConfig (Env m) where
  getSimpleHandlerConfig = simpleHandlerConfig

instance HasLogger (Env Handler) (RT Handler) where
  getLogger = logger
