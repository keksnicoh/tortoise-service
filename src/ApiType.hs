{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import           Servant.API
import Servant.API.WebSocket
import           Data.Time
import           Content.Monitor
import           Content.Switch
import           Content.Status
import           Content.TimeSeries
import           Servant.Multipart

type SetStatusAPI = ReqBody '[JSON] StatusRequest :> Post '[JSON] Status
type GetStatusAPI = Get '[JSON] [Status]
type GetTimeSeriesAPI
  = QueryParam "start" UTCTime :> QueryParam "end" UTCTime :> Get '[JSON] TimeSeries
type PostSwitchAPI = ReqBody '[JSON] SwitchRequest :> PostNoContent '[JSON] ()
type GetMonitorAPI = Get '[JSON] Monitor
type WebcamAPI = MultipartForm Mem (MultipartData Mem) :> PostNoContent '[JSON] ()
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