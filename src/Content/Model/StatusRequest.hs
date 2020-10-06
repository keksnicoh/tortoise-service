{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Content.Model.StatusRequest where

import qualified Core.Database.Model.Status as C
import           Core.Internal              (Humidity, Temperature)
import           Data.Aeson                 (FromJSON)
import           Data.Time                  (UTCTime)
import           Data.UUID                  (UUID)
import           GHC.Generics               (Generic)

data StatusRequest
  = StatusRequest
    { temperature        :: Maybe Temperature
    , humidity           :: Maybe Humidity
    , temperatureOutside :: Maybe Temperature
    , humidityOutside    :: Maybe Humidity
    } deriving (Generic, Show, Eq, FromJSON)

toStatus :: StatusRequest -> UUID -> UTCTime -> C.Status
toStatus (StatusRequest t h to ho) uuid = C.Status uuid t h to ho
