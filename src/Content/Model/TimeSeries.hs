{-# LANGUAGE DeriveGeneric #-}

module Content.Model.TimeSeries
  ( TimeSeries(..)
  , from
  )
where
import           Servant.Docs                   ( ToSample
                                                , toSamples
                                                , singleSample
                                                )
import           Data.Swagger
import qualified Data.Time                     as T
import           GHC.Generics                   ( Generic )
import           Data.Aeson
import           Core.Internal
import           Core.Database.Model.Status     ( Status(..) )

data TimeSeries = TimeSeries
  { xtime        :: [T.NominalDiffTime]
  , ytemperature :: [Temperature]
  , yhumidity    :: [Humidity]
  } deriving(Generic, Eq, Show)

instance ToJSON TimeSeries
instance ToSchema TimeSeries
instance ToSample TimeSeries where
  toSamples _ = singleSample $ TimeSeries [400, 300, 200, 100, 0]
                                          [23.5, 23.6, 23.7, 23.8, 24]
                                          [4, 4, 4, 3, 4]

from :: T.UTCTime -> [Status] -> TimeSeries
from start series = TimeSeries (x <$> series)
                               (temperature <$> series)
                               (humidity <$> series)
  where x status = T.diffUTCTime (created status) start
