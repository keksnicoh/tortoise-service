{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.Database.Spec where

import           Control.Exception              (SomeException, try)
import qualified Core.Database.Model.StatusSpec
import qualified Data.ByteString                as BS
import           Data.ByteString.Internal       as BSI
import           Database.PostgreSQL.Simple
import           OpenEnv
import           System.Environment             (lookupEnv)
import           Test.Hspec

databaseSpec :: IO (Maybe Spec)
databaseSpec = do
  putStrLn "connect to database..."
  envPSQL (e "PSQL_SPECS")
    >>= \case
      Nothing -> return Nothing
      Just psqlConnectionString -> try (connectPostgreSQL psqlConnectionString)
        >>= \case
          Left (x :: SomeException) -> do
            print x
            return Nothing
          Right connection          -> do
            cleanDbSql <- BS.readFile "sql/clean.sql"
            dbSchema <- BS.readFile "sql/db.sql"
            let fullSchema = [cleanDbSql, dbSchema]
                env = connection #: fullSchema #: nil
            return
              $ Just
              $ describe "Core.Database.Model.StatusSpec"
              $ Core.Database.Model.StatusSpec.mkSpec env
  where
    e v = "TORTOISE_SERVICE_" <> v

    envPSQL env = fmap BSI.packChars <$> lookupEnv env
