import qualified Network.Wai.Handler.Warp
import qualified Env
import qualified Server
import qualified Swagger
import qualified Bootstrap
--import           Control.Concurrent
--import qualified HouseMonitor                   ( start )
--import Control.Monad.Reader (MonadIO(liftIO))

main :: IO ()
main = do
  env <- Bootstrap.createEnvironment

  putStrLn "[Tortoise-Service] Start House Monitoring"
  --forkIO $ HouseMonitor.start env

  putStrLn "[Tortoise-Service] Start WAI Appplication"
  if Env.applicationMode env == Env.Development
    then Network.Wai.Handler.Warp.run (Env.port env) (Swagger.swaggerApp env)
    else Network.Wai.Handler.Warp.run (Env.port env) (Server.turtleApp env)
