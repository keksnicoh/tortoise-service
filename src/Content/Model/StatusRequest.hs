{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Content.Model.StatusRequest where

import           GHC.Generics                   ( Generic )
import           Data.Aeson
import           Servant.Docs                   ( ToSample
                                                , toSamples
                                                , singleSample
                                                )
import           Data.Swagger
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
    } deriving (Generic, Show, Eq, ToSchema, FromJSON)

toStatus :: StatusRequest -> UUID -> UTCTime -> C.Status
toStatus (StatusRequest t h to ho) uuid = C.Status uuid t h to ho

instance ToSample StatusRequest where
  toSamples _ =
    singleSample (StatusRequest (Just 13.37) Nothing (Just 5) (Just 16))
