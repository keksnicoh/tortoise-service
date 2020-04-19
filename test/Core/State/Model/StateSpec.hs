module Core.State.Model.StateSpec where

import Core.State.Model.State 
import Test.Hspec

spec :: Spec
spec =
  describe "initialState" $ 
    it "should be empty" $
      initialState `shouldBe` State Nothing Nothing