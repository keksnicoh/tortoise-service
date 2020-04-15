{-# LANGUAGE FlexibleInstances #-}
module Env where

import           Database.PostgreSQL.Simple
import           Core.Database.Env
import qualified Data.Time                     as T
import           Data.UUID                      ( UUID )

data Env = Env
  { dbConnection :: Connection
  , port :: Int
  , currentTime :: IO T.UTCTime
  , randomUUID :: IO UUID
  }
class HasCurrentTime a where getCurrentTime :: a -> IO T.UTCTime
class HasRandomUUID a where getRandomUUID :: a -> IO UUID

instance HasDbConnection Env where           getDbConnection = dbConnection
instance HasCurrentTime Env where            getCurrentTime = currentTime
instance HasCurrentTime (IO T.UTCTime) where getCurrentTime = id
instance HasRandomUUID Env where             getRandomUUID = randomUUID
