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

data Status = Status
  { statusId    :: UUID
  , temperature :: Temperature
  , humidity    :: Humidity
  , created     :: UTCTime
  } deriving (Generic, Show, Eq)

from :: C.Status -> Status
from status = Status (C.statusId status)
                     (C.temperature status)
                     (C.humidity status)
                     (C.created status)

instance ToJSON Status
instance ToSchema Status
instance ToSample Status where
  toSamples _ =
    singleSample $ Status nil 1.0 2.0 (read "2011-11-19 18:28:r52.607875 UTC")

