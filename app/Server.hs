{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Control.Monad.Reader
import           Servant
import           ApiType
import           Env

import qualified Content.Service.WebcamService as WebcamService
import qualified Content.Handler.WebcamHandler as WebcamHandler
import           Content.Service.StatusService as StatusService
import           Content.Service.MonitorService
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
import           Servant                        ( Proxy(..)
                                                , serve
                                                )


import           Stream.Service.Action


turtleServer :: ServerT TurtleAPI (ReaderT Env Handler)
turtleServer =
  tatusServer
    :<|> timeSeriesServer
    :<|> monitorServer
    :<|> controlServer
    :<|> webcamServer
    :<|> streamData CS.currentState
 where
  timeSeriesServer = TimeSeriesService.mkGroupedTimeSeriesService
    $ TimeSeriesService.mkTimeSeriesService C.fetchStatusPeriodRepository
  tatusServer = StatusService.mkPostStatusService C.insertStatusRepository
    :<|> StatusService.mkGetStatusService (C.mkFetchStatusRepository 10)
  monitorServer = MonitorService.mkMonitorService
    CS.currentState
    C.fetchStatusPeriodRepository
    COR.forecastRepository
  controlServer = SwitchService.mkSwitchService CS.updateState
  webcamServer =
    WebcamHandler.mkWebcamHandler
        (WebcamService.mkPersistWebcam "webcam.jpg" CS.updateState LBS.writeFile
        )
      :<|> WebcamService.mkRequestWebcamService CS.updateState

turtleAPI :: Proxy TurtleAPI
turtleAPI = Proxy

turtleApp :: Env -> Application
turtleApp s = serve turtleAPI $ hoistServer turtleAPI (nt s) turtleServer
  where nt s x = runReaderT x s
