{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import           Servant.API
import           Content.Status
import           Content.Model.TimeSeries
import           Data.Time
import Content.Model.MonitorResult
import Content.Model.SwitchRequest

type SetStatusAPI = ReqBody '[JSON] StatusRequest :> Post '[JSON] Status
type GetStatusAPI = Get '[JSON] [Status]
type GetTimeSeriesAPI
  = QueryParam "start" UTCTime :> QueryParam "end" UTCTime :> Get '[JSON] TimeSeries
type PostSwitchAPI = ReqBody '[JSON] SwitchRequest :> PostNoContent '[JSON] ()
type GetMonitorAPI = Get '[JSON] MonitorResult

type StatusAPI     = SetStatusAPI :<|> GetStatusAPI
type TimeSeriesAPI = GetTimeSeriesAPI
type MonitorAPI    = GetMonitorAPI
type ControlAPI    = "switch" :> PostSwitchAPI

type TurtleAPI = 
    "v1" :> ("status"  :> StatusAPI 
        :<|> "series"  :> TimeSeriesAPI
        :<|> "monitor" :> MonitorAPI
        :<|> "control" :> ControlAPI)
