{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where 

import Servant.API
import Content.Status

type SetStatusAPI = ReqBody '[JSON] StatusRequest :> Post '[JSON] Status
type GetStatusAPI = Get '[JSON] [Status]
type TurtleAPI = "status" :> (SetStatusAPI :<|> GetStatusAPI)
