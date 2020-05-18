module Core.State.Model.StateSpec where

import           Core.State.Model.State
import           Test.Hspec

spec :: Spec
spec = describe "initialState" $ do
  it "should be empty" $ do
    initialState `shouldBe` State { light1           = Nothing
                                  , light2           = Nothing
                                  , webcamDate       = Nothing
                                  , webcamRequest    = Nothing
                                  , houseMonitor     = MonitorOff
                                  , controlLockDate1 = Nothing
                                  , controlLockDate2 = Nothing
                                  }
