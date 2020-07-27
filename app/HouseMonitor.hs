{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HouseMonitor where

import           Control.Concurrent
import qualified Automation.Service.EmergencyService
                                               as ASEmergencyService
import qualified Automation.Service.ReadSensorService
                                               as ASReadSensorService
import           Control.Monad.Reader           ( runReaderT
                                                , MonadIO(liftIO)
                                                )
import qualified Core.Database.Model.Status    as CDMStatus
                                                ( mkFetchStatusRepository )
import qualified Core.State.Repository.State   as CSRState
                                                ( currentState
                                                , updateState
                                                )
import           Core.State.Model.State         ( State )
import           Text.Printf
import           Automation.Model.HouseStateConfig
                                                ( HouseStateConfig(..) )
import           Automation.FSM.Transitions     ( FSMHandlers(..)
                                                , mkFSM
                                                )
import           Automation.FSM.HouseT          ( runHouseT )
import           Automation.Free.Interpreter
import           Automation.Service.GetLightStatusService
import           Automation.Service.ProposeSwitchLightService
import           Automation.Service.LockLightService
import           Automation.Service.SimpleControllerService
import           OpenEnv
import qualified Data.Time                     as T
import           Data.IORef                     ( IORef )
import           Database.PostgreSQL.Simple
import           Automation.Env
import           Automation.Model.SimpleHandlerConfig


start
  :: forall e
   . ( Provides HouseStateConfig e
     , Provides (IORef State) e
     , Provides SimpleHandlerConfig e
     , Provides Connection e
     , Provides FSMNRetry e
     )
  => e
  -> IO ()
start env =
  let
    delayDuration  = delaySensorRead (getValue env)
    getLightStatus = mkGetLightStatus CSRState.currentState
    proposeSwitchLight =
      mkProposeSwitchLight getLightStatus CSRState.updateState
    lockLight             = mkLockLight CSRState.updateState
    fetchStatusRepository = CDMStatus.mkFetchStatusRepository 1
    interpreter           = mkInterpreter getLightStatus
                                          proposeSwitchLight
                                          lockLight
                                          fetchStatusRepository
    controlHandler   = mkControllerHandler interpreter
    statusRepository = CDMStatus.mkFetchStatusRepository 5
    handlers         = FSMHandlers
      { readSensor      = ASReadSensorService.mkReadSensor statusRepository
      , controlTick     = controlHandler
      , emergencyAction = ASEmergencyService.mkEmergencyAction
                            CSRState.currentState
                            CSRState.updateState
      , delay           = liftIO (threadDelay delayDuration)
      }
  in
    run
      handlers
      (  getValue @HouseStateConfig env
      #: getValue @(IORef State) env
      #: getValue @FSMNRetry env
      #: getValue @Connection env
      #: getValue @SimpleHandlerConfig env
      #: T.getCurrentTime
      #: nil
      )

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
