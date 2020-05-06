module Core.Database.Env where

import           Database.PostgreSQL.Simple
import           Data.ByteString

class HasDbConnection a where
  getDbConnection :: a -> Connection

instance HasDbConnection Connection where
  getDbConnection = id

class HasDbSchema a where
  getDbSchema :: a -> [ByteString]
