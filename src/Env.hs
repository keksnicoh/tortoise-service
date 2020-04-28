{-# LANGUAGE FlexibleInstances #-}
module Env where

import           Database.PostgreSQL.Simple
import           Core.Database.Env
import qualified Data.Time                     as T
import           Data.UUID                      ( UUID )
import           Dependencies
import           Core.State.Model.State
import           Core.State.Env
import           Core.OpenWeatherMap.Env
import           GHC.IORef                      ( IORef )

data Env = Env
  { dbConnection      :: Connection
  , port              :: Int
  , currentTime       :: IO T.UTCTime
  , randomUUID        :: IO UUID
  , state             :: IORef State
  , openWeatherMapEnv :: OpenWeatherMapEnv
  , assetsPath        :: FilePath
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