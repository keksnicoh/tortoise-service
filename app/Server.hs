{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Server where

import           Control.Monad.Reader
import           Servant
import           Api

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
import           Stream.Service.Action
import           OpenEnv
import           Core.State.Model.State         ( State )

import qualified Data.Time                     as T
import           Data.IORef                     ( IORef )
import           Database.PostgreSQL.Simple
import           Data.UUID                      ( UUID )
import           Core.OpenWeatherMap.Env

type TurtleJsonServerEnv e
  = ( Provides String e
    , Embedded T.UTCTime e (ReaderT e Handler)
    , Embedded UUID e (ReaderT e Handler)
    , Provides (IORef State) e
    , Provides Connection e
    , Provides OpenWeatherMapEnv e
    )
turtleJsonServer
  :: TurtleJsonServerEnv e => ServerT TurtleJsonAPI (ReaderT e Handler)
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

type WebcamServerEnv e = Provides FilePath e
type StreamEnv e = Provides (IORef State) e
type TurtleEnv e = (TurtleJsonServerEnv e, WebcamServerEnv e, StreamEnv e)
turtleServer
  :: TurtleEnv e
  => ServerT TurtleAPI (ReaderT e Handler)
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

turtleApp :: TurtleEnv e => e -> Application
turtleApp s = serve turtleAPI $ hoistServer turtleAPI (nt s) turtleServer
  where nt s x = runReaderT x s
