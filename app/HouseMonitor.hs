{-# LANGUAGE LambdaCase #-}

module HouseMonitor where

import           Control.Concurrent
import qualified Automation.Env                as AEnv
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
import           Text.Printf
import           Automation.Model.HouseStateConfig
                                                ( HouseStateConfig(..)
                                                , HasHouseStateConfig(..)
                                                )
import           Automation.FSM.Transitions     ( FSMHandlers(..)
                                                , mkFSM
                                                )
import           Automation.FSM.HouseT          ( runHouseT )
import           Automation.Free.Interpreter
import           Automation.Service.GetLightStatusService
import           Automation.Service.ProposeSwitchLightService
import           Automation.Service.LockLightService
import           Automation.Service.SimpleControllerService
import qualified Env                           as E


{-
  :: (Monad m)
  => GetLightStatus m
  -> ProposeSwitchLight m
  -> LockLight m
  -> CDMStatus.FetchStatusRepository m
  -> SimpleControllerInterpreter a m-}

start :: MonadIO m => E.Env m -> IO ()
start env =
  let
    automationEnv  = AEnv.fromMainEnv env :: AEnv.AutomationEnvironment IO
    delayDuration  = delaySensorRead (getHouseStateConfig automationEnv)
    getLightStatus = mkGetLightStatus CSRState.currentState
    proposeSwitchLight =
      mkProposeSwitchLight getLightStatus CSRState.updateState
    lockLight             = mkLockLight CSRState.updateState
    fetchStatusRepository = CDMStatus.mkFetchStatusRepository 1
    interpreter           = mkInterpreter getLightStatus
                                          proposeSwitchLight
                                          lockLight
                                          fetchStatusRepository
    statusRepository  = CDMStatus.mkFetchStatusRepository 5
    handlers          = FSMHandlers
      { readSensor      = ASReadSensorService.mkReadSensor statusRepository
      , controlTick     = mkControllerHandler interpreter
      , emergencyAction = ASEmergencyService.mkEmergencyAction
                            CSRState.currentState
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
