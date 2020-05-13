{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module HouseMonitor where

import           Control.Concurrent
import qualified Automation.HouseState         as AHouseState
import qualified Automation.Env                as AEnv
                                                ( HasHouseStateConfig(..)
                                                , HouseStateConfig(..)
                                                )
import           Core.FSM
import           Control.Monad.Reader           ( ReaderT
                                                , MonadReader
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
import           Env

type HRT m = HouseT (ReaderT (Env m) m)


instance D.HasCurrentTime (Env IO) (HRT IO) where
  getCurrentTime a = undefined




start :: MonadIO m => Env m -> IO ()
start env = do
  --let foerk = createFSMHandlers env
  undefined


run handlers env = runReaderT (runHouseT $ mkFSM handlers) env >>= \case
  reason -> do
    --debug reason
    liftIO $ threadDelay $ 120 * 1000000
    run handlers env
 where
  debug r = liftIO $ putStrLn $ printf
    "[Tortoise-Service] Terminating %s. Restart in 120 seconds..."
    (show r)



createFSMHandlers config = FSMHandlers
  { readSensor = AHouseState.mkReadSensor (CDMStatus.mkFetchStatusRepository 5)
  , controlTick     = liftIO $ putStrLn "controlling :)"
  , emergencyAction = AHouseState.mkEmergencyAction CSRState.currentState
                                                    CSRState.updateState
  , delay           =
    liftIO
      $ threadDelay (AEnv.delaySensorRead $ AEnv.getHouseStateConfig config)
  }
