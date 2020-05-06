{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Control.Monad.Reader
import           Servant
import           Api
import           Env

import qualified Content.Service.WebcamService as WebcamService
import qualified Content.Handler.WebcamHandler as WebcamHandler
import qualified Content.Service.StatusService as StatusService
import qualified Content.Service.MonitorService
                                               as MonitorService
import qualified Content.Service.TimeSeriesService
                                               as TimeSeriesService
import qualified Content.Service.SwitchService as SwitchService

import qualified Core.Database.Model.Status    as C
import qualified Core.State.Repository.State   as CS
import qualified Core.OpenWeatherMap.Repository.Forecast
                                               as COR
import qualified Data.ByteString.Lazy          as LBS
import           Network.Wai                    ( Application )
import           Stream.Service.Action

turtleJsonServer :: ServerT TurtleJsonAPI (ReaderT Env Handler)
turtleJsonServer =
  statusServer :<|> timeSeriesServer :<|> monitorServer :<|> controlServer
 where
  statusServer     = postStatusService :<|> getStatusService
  controlServer    = SwitchService.mkSwitchService CS.updateState
  monitorServer    = monitorService
  timeSeriesServer = groupedTimeSeriesService

  groupedTimeSeriesService =
    TimeSeriesService.mkGroupedTimeSeriesService timeSeriesService
  monitorService = MonitorService.mkMonitorService
    CS.currentState
    C.fetchStatusPeriodRepository
    COR.forecastRepository
  timeSeriesService =
    TimeSeriesService.mkTimeSeriesService C.fetchStatusPeriodRepository
  postStatusService =
    StatusService.mkPostStatusService C.insertStatusRepository
  getStatusService =
    StatusService.mkGetStatusService (C.mkFetchStatusRepository 10)

turtleServer :: ServerT TurtleAPI (ReaderT Env Handler)
turtleServer =
  turtleJsonServer :<|> webcamServer :<|> streamData CS.currentState
 where
  webcamServer =
    WebcamHandler.mkWebcamHandler
        (WebcamService.mkPersistWebcam "webcam.jpg" CS.updateState LBS.writeFile
        )
      :<|> WebcamService.mkRequestWebcamService CS.updateState

turtleJsonAPI :: Proxy TurtleJsonAPI
turtleJsonAPI = Proxy

turtleWebcamAPI :: Proxy TurtleWebcamAPI
turtleWebcamAPI = Proxy

turtleWebsocketsAPI :: Proxy TurtleWebsocketsAPI
turtleWebsocketsAPI = Proxy

turtleAPI :: Proxy TurtleAPI
turtleAPI = Proxy

turtleApp :: Env -> Application
turtleApp s = serve turtleAPI $ hoistServer turtleAPI (nt s) turtleServer
  where nt s x = runReaderT x s
