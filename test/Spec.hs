{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import qualified Content.Model.StatusSpec      as ModelStatusSpec
import qualified Content.Service.StatusSpec    as ServiceStatusSpec
import qualified Content.Service.TimeSeriesSpec
                                               as TimeSeriesSpec
import qualified Content.Model.StatusRequestSpec
                                               as StatusRequestSpec
import qualified Core.Database.Model.StatusSpec
                                               as CDMStatusSpec
import qualified Core.State.Model.StateSpec    as CSMStateSpec
import qualified Core.State.EnvSpec            as CSEnvSpec
import qualified Core.State.Repository.StateSpec
                                               as CSRStateSpec

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
    describe "Core.Database.Model.Status" $ CDMStatusSpec.mkSpec env
    describe "Core.State.Model.State"      CSRStateSpec.spec
    describe "Core.State.Repository.State" CSMStateSpec.spec
    describe "Core.State.Env"              CSEnvSpec.spec
    describe "Content.Model.Status"        ModelStatusSpec.spec
    describe "Content.Model.StatusRequest" StatusRequestSpec.spec
    describe "Content.Service.Status"      ServiceStatusSpec.spec
    describe "Content.Service.TimeSeries"  TimeSeriesSpec.spec
