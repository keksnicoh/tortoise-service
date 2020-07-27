import qualified Network.Wai.Handler.Warp
import qualified Server
import qualified Swagger
import qualified Bootstrap
import           Control.Concurrent
import qualified HouseMonitor                   ( start )
import           ApplicationMode
import           OpenEnv

main :: IO ()
main = do
  env <- Bootstrap.createEnvironment

  putStrLn "[Tortoise-Service] Start House Monitoring"
  forkIO $ HouseMonitor.start env

  putStrLn "[Tortoise-Service] Start WAI Appplication"
  if getValue env == Development
    then Network.Wai.Handler.Warp.run (getValue env) (Swagger.swaggerApp env)
    else Network.Wai.Handler.Warp.run (getValue env) (Server.turtleApp env)
