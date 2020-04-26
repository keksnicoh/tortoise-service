module Stream.Service.Action where

import qualified Core.State.Repository.State   as CS
import           Control.Concurrent             ( threadDelay )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Foldable                  ( forM_ )

import           Network.WebSockets             ( Connection
                                                , forkPingThread
                                                , sendTextData
                                                )



import           Core.State.Env                 ( HasState(..) )
import           Control.Monad.Reader           ( MonadReader
                                                , when
                                                )
import           Data.IORef                     ( newIORef
                                                , readIORef
                                                , atomicModifyIORef'
                                                )
import           Data.Aeson                     ( encode )
import           Stream.Model.LightState
import           Stream.Model.Action

streamData
  :: (MonadIO m, MonadReader e m, HasState e)
  => CS.GetState m
  -> Connection
  -> m ()
streamData getCurrentState c = do
  currentState <- getCurrentState
  let lightState = fromState currentState
  lastStateIoRef <- liftIO $ newIORef lightState

  liftIO $ do
    putStrLn "New Stream Client..."
    sendTextData c (encode $ LightAction lightState)
    forkPingThread c 10

  forM_ [1 ..] $ \i -> do

    state <- fromState <$> getCurrentState
    liftIO $ do
      lastState <- readIORef lastStateIoRef
      when (state /= lastState) $ do
        sendTextData c (encode $ LightAction state)
        atomicModifyIORef' lastStateIoRef $ const (state, ())

      threadDelay 100000
