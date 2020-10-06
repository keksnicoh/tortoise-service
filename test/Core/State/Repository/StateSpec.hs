module Core.State.Repository.StateSpec where

import           Control.Monad.Reader        (ReaderT (runReaderT))
import           Core.State.Model.State      (HouseMonitor (MonitorOK, MonitorOff),
                                              State (State),
                                              Switch (Controlled, Manual),
                                              initialState)
import           Core.State.Repository.State (updateState)
import           Data.IORef                  (newIORef, readIORef)
import           OpenEnv                     (nil, ( #: ))
import           Test.Hspec                  (Spec, describe, it, shouldBe)
spec :: Spec
spec = do
  describe "updateState" $ do
    it "should update the state using the given function" $ do
      let newState0 = initialState
          newState1 = State (Just (Controlled True))
                            (Just (Controlled False))
                            Nothing
                            Nothing
                            MonitorOff
                            Nothing
                            Nothing
          newState2 = State (Just (Controlled False))
                            (Just (Manual False))
                            Nothing
                            Nothing
                            MonitorOK
                            Nothing
                            (Just (read "2019-02-03 13:37:42Z"))
          f1 x | x == newState0 = newState1
          f2 x | x == newState1 = newState2

      ioRef <- newIORef newState0
      runReaderT (updateState f1) (ioRef #: nil)
      readIORef ioRef >>= flip shouldBe newState1
      runReaderT (updateState f2) (ioRef #: nil)
      readIORef ioRef >>= flip shouldBe newState2
