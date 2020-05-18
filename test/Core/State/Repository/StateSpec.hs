module Core.State.Repository.StateSpec where

import           Core.State.Repository.State
import           Core.State.Model.State
import           Test.Hspec
import           Data.IORef                     ( readIORef
                                                , newIORef
                                                )
import           Control.Monad.Reader           ( ReaderT(runReaderT) )

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
                            (Just (read "2019-02-03 13:37:42"))
          f1 x | x == newState0 = newState1
          f2 x | x == newState1 = newState2

      ioRef <- newIORef newState0
      runReaderT (updateState f1) ioRef
      readIORef ioRef >>= flip shouldBe newState1
      runReaderT (updateState f2) ioRef
      readIORef ioRef >>= flip shouldBe newState2
