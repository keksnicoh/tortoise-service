{-# LANGUAGE FlexibleInstances #-}

module Env
  ( Env(..)
  , ApplicationMode(..)
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
import qualified Automation.Env as AEnv

data ApplicationMode
  = Development
  | Staging
  | Production
  deriving (Eq, Show)

data Env
  = Env
    { applicationMode :: ApplicationMode
    , dbConnection :: Connection
    , port :: Int
    , currentTime :: IO T.UTCTime
    , randomUUID :: IO UUID
    , state :: IORef State
    , openWeatherMapEnv :: OpenWeatherMapEnv
    , assetsPath :: FilePath
    , houseStateConfig :: AEnv.HouseStateConfig
    }

instance HasDbConnection Env where
  getDbConnection = dbConnection

instance HasCurrentTime Env  where
  getCurrentTime = currentTime

instance HasRandomUUID Env where
  getRandomUUID = randomUUID

instance HasState Env where
  getState = state

instance HasOpenWeatherMapEnv Env where
  getOpenWeatherMapEnv = openWeatherMapEnv

instance HasAssetsPath Env where
  getAssetsPath = assetsPath

instance AEnv.HasHouseStateConfig Env where
  getHouseStateConfig = houseStateConfig