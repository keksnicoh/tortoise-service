{-# LANGUAGE FlexibleInstances #-}
module Env where

import           Database.PostgreSQL.Simple
import           Core.Database.Env
import qualified Data.Time                     as T
import           Data.UUID                      ( UUID )
import           Dependencies

data Env = Env
  { dbConnection :: Connection
  , port         :: Int
  , currentTime  :: IO T.UTCTime
  , randomUUID   :: IO UUID
  }

instance HasDbConnection Env where getDbConnection = dbConnection
instance HasCurrentTime Env  where getCurrentTime = currentTime
instance HasRandomUUID Env   where getRandomUUID = randomUUID
