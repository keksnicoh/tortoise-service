import qualified Network.Wai.Handler.Warp
import qualified Env
import qualified Server
import qualified Swagger
import qualified Bootstrap

main :: IO ()
main = do
  env <- Bootstrap.createEnvironment
  if Env.applicationMode env == Env.Development
    then Network.Wai.Handler.Warp.run (Env.port env) (Swagger.swaggerApp env)
    else Network.Wai.Handler.Warp.run (Env.port env) (Server.turtleApp env)
