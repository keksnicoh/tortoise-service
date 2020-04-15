module Server where
  
import           Control.Monad.Reader
import           Network.Wai
import           Servant
import           ApiType
import           Env
import           Content.Service.Status
import qualified Core.Database.Model.Status    as C


turtleServer :: ServerT TurtleAPI (ReaderT Env Handler)
turtleServer = mkPostStatusService C.insertStatusRepository
  :<|> mkGetStatusService (C.mkFetchStatusRepository 10)

turtleAPI :: Proxy TurtleAPI
turtleAPI = Proxy

turtleApp :: Env -> Application
turtleApp s = serve turtleAPI $ hoistServer turtleAPI (nt s) turtleServer
  where nt s x = runReaderT x s
