module Core.State.Model.StateSpec where

import           Core.State.Model.State (HouseMonitor (MonitorOff), State (State, controlLockDate1, controlLockDate2, houseMonitor, light1, light2, webcamDate, webcamRequest),
                                         initialState)
import           Test.Hspec             (Spec, describe, it, shouldBe)

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
