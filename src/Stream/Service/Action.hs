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
import           Data.Aeson                     ( encode )
import           Stream.Model.LightState
import           Stream.Model.Action
import qualified Dependencies                  as D
import           Data.Time
import qualified Data.ByteString               as BS
import           Core.State.Model.State
import           Control.Monad.State.Strict     ( runStateT
                                                , modify'
                                                , StateT
                                                , gets
                                                )

data ActionEnv
  = ActionEnv
    { lastLightStateC :: LightState
    , pingTimeC :: UTCTime
    , lightTimeC :: UTCTime
    , connectionC :: Connection
    , dispatchTimeC :: UTCTime
    , dispatchStateC :: State
    , webcamRequestDateC :: UTCTime
    }

data ActionResult = Continue | Exit

streamData
  :: (MonadIO m, MonadReader e m, D.HasCurrentTime e)
  => CS.GetState m
  -> Connection
  -> m ()
streamData getCurrentState connection = do
  liftIO $ putStrLn "new client"
  currentTime  <- reader D.getCurrentTime >>= liftIO
  currentState <- getCurrentState
  run ActionEnv { lastLightStateC    = fromState currentState
                , pingTimeC          = currentTime
                , lightTimeC         = addUTCTime (-300) currentTime
                , connectionC        = connection
                , dispatchTimeC      = currentTime
                , dispatchStateC     = currentState
                , webcamRequestDateC = currentTime
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
      (Exit, a) -> liftIO $ putStrLn "exit..."

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
  connection     <- gets connectionC
  when (state /= lastLightState) $ do
    liftIO $ sendTextData connection (encode $ LightChangedAction state)
    modify' (\s -> s { lastLightStateC = state, lightTimeC = dispatchTimeC s })
    liftIO $ putStrLn "[light_changed] state send"

switchAction :: StateT ActionEnv IO ()
switchAction = do
  now       <- gets dispatchTimeC
  lightTime <- gets lightTimeC
  when (diffUTCTime now lightTime > 60) $ do
    state      <- fromState <$> gets dispatchStateC
    connection <- gets connectionC
    liftIO $ sendTextData connection (encode $ LightAction state)
    modify' (\s -> s { lastLightStateC = state, lightTimeC = now })
    liftIO $ putStrLn "[light] state send"

webcamRequestAction :: StateT ActionEnv IO ()
webcamRequestAction = do
  webcamRequestDate <- gets webcamRequestDateC
  webcamRequest <$> gets dispatchStateC >>= \case
    Nothing   -> return ()
    Just date -> when (webcamRequestDate < date) $ do
      connection <- gets connectionC
      liftIO $ sendTextData connection (encode WebcamAction)
      modify' (\s -> s { webcamRequestDateC = dispatchTimeC s })
      liftIO $ putStrLn "[webcam] requested"

pingPongAction :: StateT ActionEnv IO ActionResult
pingPongAction = do
  now      <- gets dispatchTimeC
  pingTime <- gets pingTimeC
  if diffUTCTime now pingTime > 10
    then do
      connection     <- gets connectionC
      pingPongResult <- liftIO $ do
        sendTextData connection (encode PingAction)
        receiveData connection :: IO BS.ByteString
      modify' (\s -> s { pingTimeC = now })
      if pingPongResult == "pong"
        then do
          liftIO $ putStrLn "[ping] pong!"
          return Continue
        else do
          liftIO $ putStrLn "[ping] no pong, terminating..."
          return Exit
    else return Continue
