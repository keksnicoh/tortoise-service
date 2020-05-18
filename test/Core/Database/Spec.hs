{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Database.Spec where

import           Test.Hspec
import qualified Core.Database.Model.StatusSpec
import           Database.PostgreSQL.Simple
import           SpecEnv
import qualified Data.ByteString               as BS
import           Data.ByteString.Internal      as BSI
import           System.Environment             ( lookupEnv )

databaseSpec :: IO Spec
databaseSpec = do
  putStrLn "connect to database..."
  psqlConnectionString <- envPSQL (e "PSQL_SPECS")
  dbConnection         <- connectPostgreSQL psqlConnectionString
  cleanDbSql           <- BS.readFile "sql/clean.sql"
  dbSchema             <- BS.readFile "sql/db.sql"
  let fullSchema = [cleanDbSql, dbSchema]
  let env = Env { dbConnection = dbConnection, dbSchema = fullSchema }
  return $ do
    describe "Core.Database.Model.StatusSpec"
      $ Core.Database.Model.StatusSpec.mkSpec env
 where
  e v = "TORTOISE_SERVICE_" <> v
  requiredEnv env = lookupEnv env >>= \case
    Just val -> return val
    Nothing  -> error ("missing environment: " <> env)
  envPSQL env = BSI.packChars <$> requiredEnv env
