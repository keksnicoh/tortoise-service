module Server where

import           Control.Monad.Reader
import           Network.Wai
import           Servant
import           ApiType
import           Env
import           Content.Service.Status
import           Content.Service.TimeSeries
import           Content.Service.Monitor
import           Content.Service.Switch
import qualified Core.Database.Model.Status    as C
import qualified Core.State.Repository.State    as CS
import qualified Core.OpenWeatherMap.Repository.Forecast as COR
import Network.HTTP.Client (httpLbs)

turtleServer :: ServerT TurtleAPI (ReaderT Env Handler)
turtleServer = turtleStatusServer :<|> timeSeriesServer :<|> monitorServer :<|> controlServer
 where
  timeSeriesServer   = mkTimeSeriesService C.fetchStatusPeriodRepository
  turtleStatusServer = mkPostStatusService C.insertStatusRepository
    :<|> mkGetStatusService (C.mkFetchStatusRepository 10)
  monitorServer = mkMonitorService CS.currentState C.fetchStatusPeriodRepository COR.forecastRepository
  controlServer = mkSwitchService CS.updateState

turtleAPI :: Proxy TurtleAPI
turtleAPI = Proxy

turtleApp :: Env -> Application
turtleApp s = serve turtleAPI $ hoistServer turtleAPI (nt s) turtleServer
  where nt s x = runReaderT x s
