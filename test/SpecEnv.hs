module SpecEnv where

import           Core.Database.Env
import           Database.PostgreSQL.Simple
import           Data.ByteString

data Env = Env {
  dbConnection :: Connection,
  dbSchema :: [ByteString]
}

instance HasDbConnection Env where
  getDbConnection = dbConnection

instance HasDbSchema Env where
  getDbSchema = dbSchema
