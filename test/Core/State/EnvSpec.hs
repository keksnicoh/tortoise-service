module Core.State.EnvSpec where

import           Core.State.Env
import           Core.State.Model.State
import           Test.Hspec
import           Data.IORef                     ( readIORef
                                                , newIORef
                                                )

spec :: Spec
spec = describe "trivial instance of HasState (IORef State)" $ do
  it "must be the identity" $ do
    let state =
          State Nothing (Just $ Controlled True) Nothing Nothing MonitorOff
    ref <- newIORef state
    readIORef (getState ref) >>= flip shouldBe state
