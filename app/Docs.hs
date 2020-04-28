{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Docs where

--import           Data.ByteString.Lazy           ( ByteString )
--import           Data.Proxy
--import           Data.Text.Lazy.Encoding        ( encodeUtf8 )
--import           Data.Text.Lazy                 ( pack )
--import           Network.HTTP.Types
--import           Network.Wai
--import           Servant.API
--import           Servant.Docs
--import           Servant.Server
--import           ApiType
--import           Server
--import           Network.Wai.Handler.Warp
--import           Servant.Swagger
--import           Data.Swagger-
--import           Servant.Server.StaticFiles

--apiDocs :: API
--apiDocs = docs turtleAPI

--docsBS :: ByteString
--docsBS = encodeUtf8 . pack . markdown $ docsWithIntros [intro] turtleAPI

-- where
--  intro = DocIntro "Welcome" ["This is our super webservice's API.", "Enjoy!"]

--type DocsAPI = TurtleAPI :<|> Raw
--type SwaggerDistAPI = "swagger-ui" :> Raw
--type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger
--type SwaggerTurtleApi = TurtleAPI:<|> SwaggerAPI :<|> SwaggerDistAPI

--api :: Proxy DocsAPI
--api :: Proxy t
--api = Proxy

--server :: Server DocsAPI
--server = Server.turtleServer :<|> Tagged serveDocs where
--  serveDocs _ respond = respond $ responseLBS ok200 [plain] docsBS
--  plain = ("Content-Type", "text/plain")

--app :: Application
--app = serve api server

--turtleDocsMain :: IO ()
--turtleDocsMain = run 8081 app

--turtleSwagger :: Swagger
----turtleSwagger = toSwagger turtleAPI

--turtleSwaggerServer :: Server SwaggerTurtleApi
--turtleSwaggerServer :: a1 :<|> (Handler a2 :<|> Tagged m Application)
--turtleSwaggerServer =
--  error "sry" :<|> return turtleSwagger :<|> serveDirectoryWebApp
--    "swagger-ui-dist"

--  Server.turtleServer :<|> return turtleSwagger :<|> serveDirectoryWebApp
--    "swagger-ui-dist"
--swaggerAPI :: Proxy SwaggerTurtleApi
--swaggerAPI = Proxy

--swaggerApp :: Application
--swaggerApp = serve swaggerAPI turtleSwaggerServer

--swaggerMain :: IO ()
--swaggerMain = run 8081 swaggerApp
