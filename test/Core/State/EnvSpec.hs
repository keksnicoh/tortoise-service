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
    ref <- newIORef initialState
    readIORef (getState ref) >>= flip shouldBe initialState
