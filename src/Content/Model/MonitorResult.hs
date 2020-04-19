{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Content.Model.MonitorResult
  ( MonitorResult(..)
  , from
  )
where

import           Content.Model.Monitor          ( Monitor(..) )
import           Data.Aeson                     ( ToJSON )
import           GHC.Generics                   ( Generic )

data MonitorResult
  = MonitorResult
    { error :: Maybe String
    , monitor :: Maybe Monitor
    }
  deriving (Show, Eq, Generic, ToJSON)

type MonitorService m = m MonitorResult

from :: Either String Monitor -> MonitorResult
from (Left  err    ) = MonitorResult (Just err) Nothing
from (Right monitor) = MonitorResult Nothing (Just monitor)
