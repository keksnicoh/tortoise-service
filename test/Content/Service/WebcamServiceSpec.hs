{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Content.Service.WebcamServiceSpec where

import qualified Core.State.Model.State as State (initialState, webcamDate)
import Content.Service.WebcamService (mkPersistWebcam)
import Data.Time (UTCTime)
import Test.Hspec (shouldBe, Spec, describe, it)
import Dependencies (HasAssetsPath(..), HasCurrentTime(..))
import Data.IORef (newIORef, modifyIORef, readIORef)
import Control.Monad.Reader (liftIO, ReaderT(runReaderT))

type RT = ReaderT TestEnv IO
data TestEnv = TestEnv (RT UTCTime) FilePath

instance HasCurrentTime TestEnv RT where
  getCurrentTime (TestEnv t _) = t

instance HasAssetsPath TestEnv where
  getAssetsPath (TestEnv _ a) = a

newtype Mock a = Mock a

spec :: Spec
spec = 
  describe "mkPersistWebcam" $
    it "should persist the file and update state" $ do
      ioRef <- newIORef []
      let
        now         = read "2019-03-04 13:37:42"
        currentTime = return now 
        assetsPath  = "a/b/"
        env         = TestEnv currentTime assetsPath
        mockedUpdateState f = do
          liftIO (State.webcamDate (f State.initialState) `shouldBe` Just now)
          return ()
        writeFile path payload = modifyIORef ioRef $ \r -> r ++ [(path, payload)]
        service = mkPersistWebcam "test.jpg" mockedUpdateState writeFile
        effect  = service "foo-payload"

      runReaderT effect env

      fileArgs <- readIORef ioRef
      fileArgs `shouldBe` [("a/b/test.jpg", "foo-payload")]
