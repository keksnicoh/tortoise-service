{-# LANGUAGE OverloadedStrings #-}

import           Network.Wai.Handler.Warp
import           Database.PostgreSQL.Simple
import           Env
import           Server
import           Data.UUID.V4                   ( nextRandom )
import qualified Data.Time                     as T
                                                ( getCurrentTime )
main :: IO ()
main = do
  putStrLn "connect to database..."
  dbConnection <- connectPostgreSQL
    "host='localhost' user='postgres' password='docker' dbname='test'"
  let env = Env { dbConnection = dbConnection
                , port         = 8081
                , currentTime  = T.getCurrentTime
                , randomUUID   = nextRandom
                }
  run (port env) (turtleApp env)
