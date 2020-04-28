{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Content.Model.Status where

import           GHC.Generics                   ( Generic )
import           Data.Aeson
import           Servant.Docs                   ( ToSample
                                                , toSamples
                                                , singleSample
                                                )
import           Data.Swagger
import           Core.Internal
import qualified Core.Database.Model.Status    as C
import           Data.UUID
import           Data.Time

data Status
  = Status
    { statusId :: UUID
    , temperature :: Maybe Temperature
    , humidity :: Maybe Humidity
    , temperature_outside :: Maybe Temperature
    , humidity_outside :: Maybe Humidity
    , created :: UTCTime
    } deriving (Generic, Show, Eq, ToJSON, ToSchema)

from :: C.Status -> Status
from status = Status (C.statusId status)
                     (C.temperature status)
                     (C.humidity status)
                     (C.temperature_outside status)
                     (C.humidity_outside status)
                     (C.created status)

instance ToSample Status where
  toSamples _ = singleSample $ Status
    nil
    (Just 1.0)
    (Just 2.0)
    (Just 3)
    (Just 5)
    (read "2011-11-19 18:28:r52.607875 UTC")

