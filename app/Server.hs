{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Control.Monad.Reader
import           Network.Wai
import           Servant
import           ApiType
import           Env
import           Servant.API.WebSocket

import           Content.Webcam                as Webcam
import           Content.Status                as Status
import           Content.Monitor               as Monitor
import           Content.TimeSeries            as TimeSeries
import           Content.Switch                as Switch

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
  turtleStatusServer
    :<|> timeSeriesServer
    :<|> monitorServer
    :<|> controlServer
    :<|> webcamServer
    :<|> streamData CS.currentState
 where
  timeSeriesServer =
    TimeSeries.mkTimeSeriesService C.fetchStatusPeriodRepository
  turtleStatusServer = Status.mkPostStatusService C.insertStatusRepository
    :<|> Status.mkGetStatusService (C.mkFetchStatusRepository 10)
  monitorServer = Monitor.mkMonitorService CS.currentState
                                           C.fetchStatusPeriodRepository
                                           COR.forecastRepository
  controlServer = Switch.mkSwitchService CS.updateState
  webcamServer  = Webcam.mkWebcamHandler
    (Webcam.mkPersistWebcam "webcam.jpg" CS.updateState LBS.writeFile)

turtleAPI :: Proxy TurtleAPI
turtleAPI = Proxy

turtleApp :: Env -> Application
turtleApp s = serve turtleAPI $ hoistServer turtleAPI (nt s) turtleServer
  where nt s x = runReaderT x s
