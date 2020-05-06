{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Content.Model.StatusRequest where

import           GHC.Generics                   ( Generic )
import           Data.Aeson
import           Core.Internal
import           Data.UUID
import qualified Core.Database.Model.Status    as C
import           Data.Time                      ( UTCTime )

data StatusRequest
  = StatusRequest
    { temperature :: Maybe Temperature
    , humidity :: Maybe Humidity
    , temperatureOutside :: Maybe Temperature
    , humidityOutside :: Maybe Humidity
    } deriving (Generic, Show, Eq, FromJSON)

toStatus :: StatusRequest -> UUID -> UTCTime -> C.Status
toStatus (StatusRequest t h to ho) uuid = C.Status uuid t h to ho
