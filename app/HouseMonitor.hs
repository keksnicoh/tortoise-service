{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module HouseMonitor where

import           Control.Concurrent
import qualified Automation.HouseState         as AHouseState
import qualified Automation.Env                as AEnv
                                                ( HasHouseStateConfig(..)
                                                , HouseStateConfig(..)
                                                )
import           Core.FSM
import           Control.Monad.Reader           ( MonadReader
                                                , runReaderT
                                                , MonadIO(liftIO)
                                                )
import qualified Core.Database.Model.Status    as CDMStatus
                                                ( mkFetchStatusRepository )
import qualified Core.Database.Env             as CDEnv
                                                ( HasDbConnection )
import qualified Core.State.Env                as CSEnv
                                                ( HasState )
import qualified Core.State.Repository.State   as CSRState
                                                ( currentState
                                                , updateState
                                                )
import qualified Dependencies                  as D
import           Text.Printf

start
  :: ( MonadIO m
     , D.HasCurrentTime e
     , CDEnv.HasDbConnection e
     , CSEnv.HasState e
     , AEnv.HasHouseStateConfig e
     )
  => e
  -> m ()
start env = run $ createFSMHandlers env
 where
  run handlers = runReaderT (runHouseT $ mkFSM handlers) env >>= \case
    reason -> do
      debug reason
      liftIO $ threadDelay $ 120 * 1000000
      run handlers
   where
    debug r = liftIO $ putStrLn $ printf
      "[Tortoise-Service] Terminating %s. Restart in 120 seconds..."
      (show r)

createFSMHandlers
  :: ( AEnv.HasHouseStateConfig e
     , D.HasCurrentTime e
     , CDEnv.HasDbConnection e
     , CSEnv.HasState e
     , MonadIO m
     , MonadReader e m
     )
  => e
  -> FSMHandlers m
createFSMHandlers config = FSMHandlers
  { readSensor = AHouseState.mkReadSensor (CDMStatus.mkFetchStatusRepository 5)
  , controlTick     = liftIO $ putStrLn "controlling :)"
  , emergencyAction = AHouseState.mkEmergencyAction CSRState.currentState
                                                    CSRState.updateState
  , delay           =
    liftIO
      $ threadDelay (AEnv.delaySensorRead $ AEnv.getHouseStateConfig config)
  }
