{-# LANGUAGE LambdaCase #-}
import qualified Network.Wai.Handler.Warp
import qualified Env
import qualified Server
import qualified Swagger
import qualified Bootstrap
import           Control.Concurrent
import           Automation.HouseState
import           Core.FSM
import           Control.Monad.Reader           ( runReaderT
                                                , MonadIO(liftIO)
                                                )
import qualified Core.Database.Model.Status    as CDMStatus
import qualified Core.State.Repository.State   as CSRState


main :: IO ()
main = do
  env <- Bootstrap.createEnvironment

  forkIO $ fsmMain env

  if Env.applicationMode env == Env.Development
    then Network.Wai.Handler.Warp.run (Env.port env) (Swagger.swaggerApp env)
    else Network.Wai.Handler.Warp.run (Env.port env) (Server.turtleApp env)


fsmMain env = do
  let config =  getHouseStateConfig env
  run FSMHandlers
      { readSensor      = mkReadSensor (CDMStatus.mkFetchStatusRepository 5)
      , controlTick     = liftIO $ putStrLn "controlling :)"
      , emergencyAction = mkEmergencyAction CSRState.currentState
                                            CSRState.updateState
      , nRetry          = 5
      , delay           = liftIO $ threadDelay (delaySensorRead config)
      }
  return ()
 where
    run handlers = runReaderT (runHouseT $ mkFSM handlers) env >>= \case
      reason -> do
        liftIO $ do 
          putStrLn $ "[HouseState] Terminating " ++ show reason ++ ". Restart in 120 seconds..."
          threadDelay $ 120 * 1000000
        run handlers

  