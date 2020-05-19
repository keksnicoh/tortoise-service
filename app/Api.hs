{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Servant.API
import           Servant.API.WebSocket
import           Data.Time
import           Content.Model.Monitor
import           Content.Model.Switch
import           Content.Model.Status
import           Content.Model.StatusRequest
import           Content.Model.TimeSeries
import           Servant.Multipart

{-- # Endpoint section: single endpoint definitions --}

{-- status endpoints --}
type SetStatusEndpoint 
  =  ReqBody '[JSON] StatusRequest 
  :> Post '[JSON] Status

type GetStatusEndpoint 
  =  Get '[JSON] [Status]

{-- timeseries endpoints --}
type GetTimeSeriesEndpoint
  =  QueryParam "start" UTCTime 
  :> QueryParam "end" UTCTime 
  :> QueryParam "timeWindow" NominalDiffTime 
  :> Get '[JSON] TimeSeries

{-- switch control endpoints --}
type PostSwitchEndpoint
  =  ReqBody '[JSON] SwitchRequest 
  :> PostNoContent '[JSON] ()

{-- monitoring endpoints --}
type GetMonitorEndpoint 
  =  Get '[JSON] Monitor

{-- webcam endpoints --}
type PostWebcamEndpoint 
  =  MultipartForm Mem (MultipartData Mem)
  :> PostNoContent '[JSON] ()

type RequestWebcamEndpoint 
   = PostNoContent '[JSON] ()

{-- # API section: combinations of several endpoints --}

{-- api's grouped by path namespace --}
type WebcamAPI
  =  "image"   :> PostWebcamEndpoint :<|> 
     "request" :> RequestWebcamEndpoint

type StreamAPI
  =  WebSocket

type StatusAPI
  =  SetStatusEndpoint :<|>
     GetStatusEndpoint

type TimeSeriesAPI
  =  GetTimeSeriesEndpoint

type MonitorAPI 
  =  GetMonitorEndpoint

type ControlAPI
  =  "switch" :> PostSwitchEndpoint

{-- combined apis --}
type TurtleJsonAPI
  =  "v1" :> ("status"  :> StatusAPI     :<|> 
              "series"  :> TimeSeriesAPI :<|> 
              "monitor" :> MonitorAPI    :<|> 
              "control" :> ControlAPI)

type TurtleWebcamAPI
  =  "v1" :> "webcam" :> WebcamAPI

type TurtleWebsocketsAPI 
  =  "v1" :> "stream" :> StreamAPI

{-- full api type --}
type TurtleAPI 
  = TurtleJsonAPI :<|> 
    TurtleWebcamAPI :<|> 
    TurtleWebsocketsAPI
