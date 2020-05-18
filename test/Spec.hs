{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import qualified Automation.Spec
import qualified Content.Spec
import qualified Core.Database.Spec
import qualified Core.State.Model.StateSpec
import qualified Core.State.EnvSpec
import qualified Core.State.Repository.StateSpec
import qualified Core.OpenWeatherMap.Spec
import qualified Core.State.Spec


main :: IO ()
main = do
  databaseSpec <- Core.Database.Spec.databaseSpec

  hspec $ do
    describe "Package Automation"          Automation.Spec.spec
    describe "Package Content"             Content.Spec.spec
    describe "Package Core.Database"       databaseSpec
    describe "Package Core.OpenWeatherMap" Core.OpenWeatherMap.Spec.spec
    describe "Package Core.State"          Core.State.Spec.spec


