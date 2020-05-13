{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

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
import qualified Automation.Env as AEnv
import Control.Monad.Reader (ReaderT)
import Servant

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
    , houseStateConfig :: AEnv.HouseStateConfig
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

instance AEnv.HasHouseStateConfig (Env m) where
  getHouseStateConfig = houseStateConfig

instance HasLogger (Env Handler) (RT Handler) where
  getLogger = logger
