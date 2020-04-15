{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import           Servant.API
import           Content.Status
import           Content.Model.TimeSeries
import           Data.Time

type SetStatusAPI = ReqBody '[JSON] StatusRequest :> Post '[JSON] Status
type GetStatusAPI = Get '[JSON] [Status]
type GetTimeSeriesAPI
  = QueryParam "start" UTCTime :> QueryParam "end" UTCTime :> Get '[JSON] TimeSeries

type StatusAPI = SetStatusAPI :<|> GetStatusAPI
type TimeSeriesAPI = GetTimeSeriesAPI

type TurtleAPI = 
    "v1" :> ("status" :> StatusAPI 
        :<|> "series" :> TimeSeriesAPI)
