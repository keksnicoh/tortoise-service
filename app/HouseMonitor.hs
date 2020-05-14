{-# LANGUAGE LambdaCase #-}

module HouseMonitor where

import           Control.Concurrent
import qualified Automation.HouseState         as AHouseState
import qualified Automation.Env                as AEnv
import qualified Automation.Config             as AConfig
import           Core.FSM
import           Control.Monad.Reader           ( runReaderT
                                                , MonadIO(liftIO)
                                                )
import qualified Core.Database.Model.Status    as CDMStatus
                                                ( mkFetchStatusRepository )
import qualified Core.State.Repository.State   as CSRState
                                                ( currentState
                                                , updateState
                                                )
import           Text.Printf
import qualified Env                           as E

start :: MonadIO m => E.Env m -> IO ()
start env =
  let
    automationEnv = AEnv.fromMainEnv env :: AEnv.AutomationEnvironment IO
    delayDuration =
      AConfig.delaySensorRead (AConfig.getHouseStateConfig automationEnv)
    statusRepository = CDMStatus.mkFetchStatusRepository 5
    handlers         = FSMHandlers
      { readSensor      = AHouseState.mkReadSensor statusRepository
      , controlTick     = liftIO (putStrLn "controlling :)")
      , emergencyAction = AHouseState.mkEmergencyAction CSRState.currentState
                                                        CSRState.updateState
      , delay           = liftIO (threadDelay delayDuration)
      }
  in
    run handlers automationEnv
 where
  run handlers env = runReaderT (runHouseT $ mkFSM handlers) env >>= \case
    reason -> do
      debug reason
      liftIO $ threadDelay $ 120 * 1000000
      run handlers env
   where
    debug r = liftIO $ putStrLn $ printf
      "[Tortoise-Service] Terminating %s. Restart in 120 seconds..."
      (show r)
