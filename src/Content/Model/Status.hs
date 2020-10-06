{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Content.Model.Status where

import qualified Core.Database.Model.Status as C
import           Core.Internal              (Humidity, Temperature)
import           Data.Aeson                 (ToJSON)
import           Data.Time                  (UTCTime)
import           Data.UUID                  (UUID)
import           GHC.Generics               (Generic)

data Status
  = Status
    { statusId           :: UUID
    , temperature        :: Maybe Temperature
    , humidity           :: Maybe Humidity
    , temperatureOutside :: Maybe Temperature
    , humidityOutside    :: Maybe Humidity
    , created            :: UTCTime
    }
    deriving (Generic, Show, Eq, ToJSON)

from :: C.Status -> Status
from status = Status (C.statusId status)
                     (C.temperature status)
                     (C.humidity status)
                     (C.temperatureOutside status)
                     (C.humidityOutside status)
                     (C.created status)
