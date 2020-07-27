{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Content.Service.WebcamServiceSpec where

import qualified Core.State.Model.State        as State
                                                ( initialState
                                                , webcamDate
                                                )
import           Content.Service.WebcamService  ( mkPersistWebcam )
import           Data.Time                      ( UTCTime )
import           Test.Hspec                     ( shouldBe
                                                , Spec
                                                , describe
                                                , it
                                                )
import           Data.IORef                     ( newIORef
                                                , modifyIORef
                                                , readIORef
                                                )
import           Control.Monad.Reader           ( liftIO
                                                , ReaderT(runReaderT)
                                                )
import           OpenEnv

spec :: Spec
spec =
  describe "mkPersistWebcam"
    $ it "should persist the file and update state"
    $ do
        ioRef <- newIORef []
        let
          now         = read @UTCTime "2019-03-04 13:37:42Z"
          currentTime = return @IO now
          assetsPath  = "a/b/" :: FilePath
          env         = currentTime #: assetsPath #: nil
          mockedUpdateState f = do
            liftIO (State.webcamDate (f State.initialState) `shouldBe` Just now)
            return ()
          writeFile path payload =
            modifyIORef ioRef $ \r -> r ++ [(path, payload)]
          service = mkPersistWebcam "test.jpg" mockedUpdateState writeFile
          effect  = service "foo-payload"

        runReaderT effect env

        fileArgs <- readIORef ioRef
        fileArgs `shouldBe` [("a/b/test.jpg", "foo-payload")]
