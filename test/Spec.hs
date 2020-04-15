{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import qualified Content.Model.StatusSpec      as ModelStatusSpec
import qualified Content.Service.StatusSpec    as ServiceStatusSpec
import qualified Content.Model.StatusRequestSpec
                                               as StatusRequestSpec
import qualified Core.Database.Model.StatusSpec
                                               as CStatusSpec
import           Database.PostgreSQL.Simple
import           SpecEnv
import qualified Data.ByteString               as BS

main :: IO ()
main = do
  putStrLn "connect to database..."
  dbConnection <- connectPostgreSQL
    "host='localhost' user='postgres' password='docker' dbname='test_hspec'"
  cleanDbSql <- BS.readFile "sql/clean.sql"
  dbSchema   <- BS.readFile "sql/db.sql"
  let fullSchema = [cleanDbSql, dbSchema]
  let env = Env { dbConnection = dbConnection, dbSchema = fullSchema }
  hspec $ do
    describe "Core.Database.Model.Status" $ CStatusSpec.mkSpec env
    describe "Content.Model.Status"        ModelStatusSpec.spec
    describe "Content.Model.StatusRequest" StatusRequestSpec.spec
    describe "Content.Service.Status"      ServiceStatusSpec.spec

