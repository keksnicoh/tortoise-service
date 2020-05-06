{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Swagger where

import           Env
import           Data.Proxy
import           Network.Wai
import           Servant.API
import           Servant.Server
import           Api
import           Server
import           Servant.Swagger
import           Data.Swagger
import           Servant.Server.StaticFiles
import qualified Content.Model.TimeSeries
import qualified Content.Model.Switch
import qualified Content.Model.Status
import qualified Content.Model.StatusRequest
import qualified Content.Model.Monitor
import qualified Data.Time                     as T
import           Control.Monad.Reader           ( ReaderT(runReaderT) )

type DocsAPI = TurtleAPI :<|> Raw
type SwaggerDistAPI = "swagger-ui" :> Raw
type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger
type SwaggerTurtleApi = TurtleAPI :<|> SwaggerAPI :<|> SwaggerDistAPI

instance ToSchema Content.Model.TimeSeries.TimeSeries
instance ToSchema (Content.Model.TimeSeries.Point T.UTCTime Float)
instance ToSchema Content.Model.Switch.SwitchRequest
instance ToSchema Content.Model.Switch.Switch
instance ToSchema Content.Model.Monitor.MonitorWeather
instance ToSchema Content.Model.Monitor.Monitor
instance ToSchema Content.Model.Status.Status
instance ToSchema Content.Model.StatusRequest.StatusRequest

api :: Proxy DocsAPI
api = Proxy

turtleSwagger :: Swagger
turtleSwagger = toSwagger turtleJsonAPI

turtleSwaggerServer :: ServerT SwaggerTurtleApi (ReaderT Env Handler)
turtleSwaggerServer =
  turtleServer :<|> return turtleSwagger :<|> serveDirectoryWebApp
    "swagger-ui-dist"

swaggerAPI :: Proxy SwaggerTurtleApi
swaggerAPI = Proxy

swaggerApp :: Env -> Application
swaggerApp s = serve swaggerAPI
  $ hoistServer swaggerAPI (nt s) turtleSwaggerServer
  where nt s x = runReaderT x s
