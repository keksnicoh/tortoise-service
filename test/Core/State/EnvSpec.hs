module Core.State.EnvSpec where

import           Core.State.Env         (HasState (getState))
import           Core.State.Model.State (initialState)
import           Data.IORef             (newIORef, readIORef)
import           Test.Hspec             (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "trivial instance of HasState (IORef State)" $ do
  it "must be the identity" $ do
    ref <- newIORef initialState
    readIORef (getState ref) >>= flip shouldBe initialState
