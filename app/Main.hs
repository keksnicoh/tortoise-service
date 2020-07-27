import qualified Network.Wai.Handler.Warp
import qualified Server
import qualified Swagger
import qualified Bootstrap
import           Control.Concurrent
import qualified HouseMonitor                   ( start )
import           HList
import           ApplicationMode

main :: IO ()
main = do
  env <- Bootstrap.createEnvironment

  putStrLn "[Tortoise-Service] Start House Monitoring"
  forkIO $ HouseMonitor.start env

  putStrLn "[Tortoise-Service] Start WAI Appplication"
  if get env == Development
    then Network.Wai.Handler.Warp.run (get env) (Swagger.swaggerApp env)
    else Network.Wai.Handler.Warp.run (get env) (Server.turtleApp env)
