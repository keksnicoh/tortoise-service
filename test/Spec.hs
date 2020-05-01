{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import qualified Content.Model.StatusSpec
import qualified Content.Model.TimeSeriesSpec
import qualified Content.Model.MonitorSpec
import qualified Content.Model.SwitchSpec
import qualified Content.Service.StatusSpec
import qualified Content.Service.TimeSeriesSpec
import qualified Content.Model.StatusRequestSpec
import qualified Core.Database.Model.StatusSpec
import qualified Core.State.Model.StateSpec
import qualified Core.State.EnvSpec
import qualified Core.State.Repository.StateSpec
import qualified Core.OpenWeatherMap.Model.ForecastSpec
import qualified Core.OpenWeatherMap.Repository.ForecastSpec
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
    describe "Core.Database.Model.Status"
      $ Core.Database.Model.StatusSpec.mkSpec env
    describe "Core.State.Model.State"      Core.State.Repository.StateSpec.spec
    describe "Core.State.Repository.State" Core.State.Model.StateSpec.spec
    describe "Core.State.Env"              Core.State.EnvSpec.spec
    describe "Core.OpenWeatherMap.Model.Forecast"
             Core.OpenWeatherMap.Model.ForecastSpec.spec
    describe "Core.OpenWeatherMap.Repository.Forecast"
             Core.OpenWeatherMap.Repository.ForecastSpec.spec
    describe "Content.Model.Status"         Content.Model.StatusSpec.spec
    describe "Content.Model.StatusRequest"  Content.Model.StatusRequestSpec.spec
    describe "Content.Service.Status"       Content.Service.StatusSpec.spec
    describe "Content.Service.TimeSeries"   Content.Service.TimeSeriesSpec.spec
    describe "Content.Model.SwitchSpec"     Content.Model.SwitchSpec.spec
    describe "Content.Model.MonitorSpec"    Content.Model.MonitorSpec.spec
    describe "Content.Model.TimeSeriesSpec" Content.Model.TimeSeriesSpec.spec
