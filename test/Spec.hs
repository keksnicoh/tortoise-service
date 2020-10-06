{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec               (describe, hspec, it, shouldBe)

import qualified Automation.Spec
import qualified Content.Spec

import qualified Core.Database.Spec
import qualified Core.OpenWeatherMap.Spec
import qualified Core.State.Spec

main :: IO ()
main = do
  databaseSpecOpt <- Core.Database.Spec.databaseSpec
  let pureTests = do
        describe "Package Automation"          Automation.Spec.spec
        describe "Package Content"             Content.Spec.spec
        describe "Package Core.OpenWeatherMap" Core.OpenWeatherMap.Spec.spec
        describe "Package Core.State"          Core.State.Spec.spec

  case databaseSpecOpt of
    Just databaseSpec -> hspec $ do
      pureTests
      describe "Package Core.Database" databaseSpec
    Nothing -> do
      putStrLn "SKIP DATABASE TEST"
      hspec $ do
        pureTests
        -- @todo fix this, handle missing database cases properly (show warnings etc...)
        it "database tests" $ True `shouldBe` False
