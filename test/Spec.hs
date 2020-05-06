{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import qualified Content.Model.StatusSpec
import qualified Content.Model.TimeSeriesSpec
import qualified Content.Model.MonitorSpec
import qualified Content.Model.SwitchSpec
import qualified Content.Service.WebcamServiceSpec
import qualified Content.Service.StatusServiceSpec
import qualified Content.Service.TimeSeriesServiceSpec
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
import           Data.ByteString.Internal      as BSI
import           System.Environment             ( lookupEnv )

main :: IO ()
main = do
  putStrLn "connect to database..."
  psqlConnectionString <- envPSQL (e "PSQL_SPECS")
  dbConnection         <- connectPostgreSQL psqlConnectionString
  cleanDbSql           <- BS.readFile "sql/clean.sql"
  dbSchema             <- BS.readFile "sql/db.sql"
  let fullSchema = [cleanDbSql, dbSchema]
  let env = Env { dbConnection = dbConnection, dbSchema = fullSchema }
  hspec $ do
    describe "Core.Database.Model.StatusSpec"
      $ Core.Database.Model.StatusSpec.mkSpec env
    describe "Core.State.Model.StateSpec" Core.State.Repository.StateSpec.spec
    describe "Core.State.Repository.StateSpec" Core.State.Model.StateSpec.spec
    describe "Core.State.EnvSpec" Core.State.EnvSpec.spec
    describe "Core.OpenWeatherMap.Model.ForecastSpec"
             Core.OpenWeatherMap.Model.ForecastSpec.spec
    describe "Core.OpenWeatherMap.Repository.ForecastSpec"
             Core.OpenWeatherMap.Repository.ForecastSpec.spec
    describe "Content.Model.StatusSpec" Content.Model.StatusSpec.spec
    describe "Content.Model.StatusRequestSpec"
             Content.Model.StatusRequestSpec.spec
    describe "Content.Service.StatusServiceSpec"
             Content.Service.StatusServiceSpec.spec
    describe "Content.Service.TimeSeriesServiceSpec"
             Content.Service.TimeSeriesServiceSpec.spec
    describe "Content.Model.SwitchSpec"     Content.Model.SwitchSpec.spec
    describe "Content.Model.MonitorSpec"    Content.Model.MonitorSpec.spec
    describe "Content.Model.TimeSeriesSpec" Content.Model.TimeSeriesSpec.spec
    describe "Content.Service.WebcamServiceSpec"
             Content.Service.WebcamServiceSpec.spec
 where
  e v = "TORTOISE_SERVICE_" <> v
  requiredEnv env = lookupEnv env >>= \case
    Just val -> return val
    Nothing  -> error ("missing environment: " <> env)
  envPSQL env = BSI.packChars <$> requiredEnv env
