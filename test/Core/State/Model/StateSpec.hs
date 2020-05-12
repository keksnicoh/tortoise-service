module Core.State.Model.StateSpec where

import           Core.State.Model.State
import           Test.Hspec

spec :: Spec
spec = describe "initialState" $ do
  it "should be empty" $ do
    initialState `shouldBe` State Nothing Nothing Nothing Nothing MonitorOff
