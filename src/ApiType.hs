{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import           Servant.API
import Servant.API.WebSocket
import           Data.Time
import           Content.Model.Monitor
import           Content.Model.Switch
import           Content.Model.Status
import           Content.Model.StatusRequest
import           Content.Model.TimeSeries
import           Servant.Multipart

type SetStatusAPI = ReqBody '[JSON] StatusRequest :> Post '[JSON] Status
type GetStatusAPI = Get '[JSON] [Status]
type GetTimeSeriesAPI
  =  QueryParam "start" UTCTime 
  :> QueryParam "end" UTCTime 
  :> QueryParam "timeWindow" NominalDiffTime 
  :> Get '[JSON] TimeSeries
type PostSwitchAPI = ReqBody '[JSON] SwitchRequest :> PostNoContent '[JSON] ()
type GetMonitorAPI = Get '[JSON] Monitor

type PostWebcam = MultipartForm Mem (MultipartData Mem) :> PostNoContent '[JSON] ()
type RequestWebcam = PostNoContent '[JSON] ()
type WebcamAPI = PostWebcam :<|> "request" :> RequestWebcam
type StreamAPI = WebSocket

type StatusAPI     = SetStatusAPI :<|> GetStatusAPI
type TimeSeriesAPI = GetTimeSeriesAPI
type MonitorAPI    = GetMonitorAPI
type ControlAPI    = "switch" :> PostSwitchAPI

type TurtleAPI = 
    "v1" :> ("status"  :> StatusAPI 
        :<|> "series"  :> TimeSeriesAPI
        :<|> "monitor" :> MonitorAPI
        :<|> "control" :> ControlAPI
        :<|> "webcam"  :> WebcamAPI
        :<|> "stream"  :> StreamAPI)