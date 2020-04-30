{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Content.Model.StatusSpec      as ModelStatusSpec
import           Content.Model.SwitchSpec      as CMSwitchSpec
import           Content.Service.StatusSpec    as ServiceStatusSpec
import           Content.Service.TimeSeriesSpec
                                               as TimeSeriesSpec
import           Content.Model.StatusRequestSpec
                                               as StatusRequestSpec
import           Core.Database.Model.StatusSpec
                                               as CDMStatusSpec
import           Core.State.Model.StateSpec    as CSMStateSpec
import           Core.State.EnvSpec            as CSEnvSpec
import           Core.State.Repository.StateSpec
                                               as CSRStateSpec
import           Core.OpenWeatherMap.Model.ForecastSpec
                                               as COMForecastSpec
import           Core.OpenWeatherMap.Repository.ForecastSpec
                                               as CORForecastSpec
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
    describe "Core.State.Model.State"                  CSRStateSpec.spec
    describe "Core.State.Repository.State"             CSMStateSpec.spec
    describe "Core.State.Env"                          CSEnvSpec.spec
    describe "Core.OpenWeatherMap.Model.Forecast"      COMForecastSpec.spec
    describe "Core.OpenWeatherMap.Repository.Forecast" CORForecastSpec.spec
    describe "Content.Model.Status"                    ModelStatusSpec.spec
    describe "Content.Model.StatusRequest"             StatusRequestSpec.spec
    describe "Content.Service.Status"                  ServiceStatusSpec.spec
    describe "Content.Service.TimeSeries"              TimeSeriesSpec.spec
    describe "Content.Model.SwitchSpec"                CMSwitchSpec.spec
