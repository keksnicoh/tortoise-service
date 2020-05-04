{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Stream.Service.Action where

import qualified Core.State.Repository.State   as CS
import           Control.Concurrent             ( threadDelay )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Network.WebSockets             ( Connection
                                                , sendTextData
                                                , receiveData
                                                )
import           Control.Monad.Reader           ( reader
                                                , MonadReader
                                                , when
                                                )
import           Data.Aeson                     ( ToJSON
                                                , encode
                                                )
import           Stream.Model.LightState
import           Stream.Model.Action
import qualified Dependencies                  as D
import           Data.Time
import qualified Data.ByteString.Lazy          as LBS
import           Core.State.Model.State
import           Control.Monad.State.Strict     ( MonadState
                                                , runStateT
                                                , modify'
                                                , StateT
                                                , gets
                                                )

data ActionEnv
  = ActionEnv
    { lastLightStateC :: LightState
    , pingTimeC :: UTCTime
    , lightTimeC :: UTCTime
    , dispatchTimeC :: UTCTime
    , dispatchStateC :: State
    , webcamRequestDateC :: UTCTime
    , sendC :: LBS.ByteString -> IO ()
    , receiveC :: IO LBS.ByteString
    }

data ActionResult = Continue | Exit

debug :: MonadIO m => String -> m ()
debug msg = liftIO (putStrLn msg)

send :: (MonadState ActionEnv m, MonadIO m, ToJSON a) => a -> m ()
send f = do
  sendTextData <- gets sendC
  liftIO . sendTextData . encode $ f

receive :: (MonadState ActionEnv m, MonadIO m) => m LBS.ByteString
receive = gets receiveC >>= liftIO

streamData
  :: (MonadIO m, MonadReader e m, D.HasCurrentTime e)
  => CS.GetState m
  -> Connection
  -> m ()
streamData getCurrentState connection = do
  debug "new client"
  currentTime  <- reader D.getCurrentTime >>= liftIO
  currentState <- getCurrentState
  run ActionEnv { lastLightStateC    = fromState currentState
                , pingTimeC          = currentTime
                , lightTimeC         = addUTCTime (-300) currentTime
                , dispatchTimeC      = currentTime
                , dispatchStateC     = currentState
                , webcamRequestDateC = currentTime
                , sendC              = sendTextData connection
                , receiveC           = receiveData connection
                }
 where
  run env = do
    result <- liftIO $ runStateT actions env
    case result of
      (Continue, env) -> do
        liftIO $ threadDelay 100000
        currentState <- getCurrentState
        currentTime  <- reader D.getCurrentTime >>= liftIO
        run env { dispatchStateC = currentState, dispatchTimeC = currentTime }
      (Exit, a) -> debug "exit..."

actions :: StateT ActionEnv IO ActionResult
actions = do
  switchUpdatedAction
  switchAction
  webcamRequestAction
  pingPongAction

switchUpdatedAction :: StateT ActionEnv IO ()
switchUpdatedAction = do
  state          <- fromState <$> gets dispatchStateC
  lastLightState <- gets lastLightStateC
  when (state /= lastLightState) $ do
    send (LightChangedAction state)
    updateState state
    debug "[light_changed] state send"
 where
  updateState state =
    modify' (\s -> s { lastLightStateC = state, lightTimeC = dispatchTimeC s })

switchAction :: StateT ActionEnv IO ()
switchAction = do
  now      <- gets dispatchTimeC
  timeDiff <- diffUTCTime now <$> gets lightTimeC
  when (timeDiff > 60) $ do
    state <- fromState <$> gets dispatchStateC
    send (LightAction state)
    updateState state now
    debug "[light] state send"
 where
  updateState state now =
    modify' (\s -> s { lastLightStateC = state, lightTimeC = now })

webcamRequestAction :: StateT ActionEnv IO ()
webcamRequestAction = do
  webcamRequestDate <- gets webcamRequestDateC
  webcamRequest <$> gets dispatchStateC >>= \case
    Nothing   -> return ()
    Just date -> when (webcamRequestDate < date) $ do
      send WebcamAction
      updateState
      debug "[webcam] requested"
 where
  updateState = modify' (\s -> s { webcamRequestDateC = dispatchTimeC s })

pingPongAction :: StateT ActionEnv IO ActionResult
pingPongAction = do
  timeDiff <- diffUTCTime <$> gets dispatchTimeC <*> gets pingTimeC
  if timeDiff > 10
    then do
      pingPongResponse <- send PingAction >> receive
      if pingPongResponse == "pong"
        then do
          updateState
          debug "[ping] pong!"
          return Continue
        else do
          debug "[ping] no pong, terminating..."
          return Exit
    else return Continue
  where updateState = modify' (\s -> s { pingTimeC = dispatchTimeC s })
