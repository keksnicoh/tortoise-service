{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Content.Model.Status where

import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( ToJSON )
import           Core.Internal                  ( Humidity
                                                , Temperature
                                                )
import qualified Core.Database.Model.Status    as C
import           Data.UUID                      ( UUID )
import           Data.Time                      ( UTCTime )

data Status
  = Status
    { statusId :: UUID
    , temperature :: Maybe Temperature
    , humidity :: Maybe Humidity
    , temperatureOutside :: Maybe Temperature
    , humidityOutside :: Maybe Humidity
    , created :: UTCTime
    }
    deriving (Generic, Show, Eq, ToJSON)

from :: C.Status -> Status
from status = Status (C.statusId status)
                     (C.temperature status)
                     (C.humidity status)
                     (C.temperatureOutside status)
                     (C.humidityOutside status)
                     (C.created status)
