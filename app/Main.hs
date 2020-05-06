import qualified Network.Wai.Handler.Warp
import qualified Env
import qualified Server

import qualified Bootstrap

main :: IO ()
main = do
  env <- Bootstrap.createEnvironment
  Network.Wai.Handler.Warp.run (Env.port env) (Server.turtleApp env)
