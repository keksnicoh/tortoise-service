{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
import           Data.Maybe                     ( catMaybes )


data Point a b
  = Point
    { x :: a
    , y :: b
    }
  deriving (Show, Eq, ToJSON, Generic)

data TimeSeries
  = TimeSeries
    { temperature        :: [Point T.UTCTime Temperature]
    , humidity           :: [Point T.UTCTime Humidity]
    , temperatureOutside :: [Point T.UTCTime Temperature]
    , humidityOutside    :: [Point T.UTCTime Humidity]
    } deriving(Generic, Eq, Show, ToJSON)

--instance ToSample TimeSeries where
--  toSamples _ = singleSample $ TimeSeries [400, 300, 200, 100, 0]
--                                          [23.5, 23.6, 23.7, 23.8, 24]
--                                          [4, 4, 4, 3, 4]

from :: T.UTCTime -> [Status] -> TimeSeries
from start series = TimeSeries (t <$> series)
                               (t2 <$> series)
                               (catMaybes $ t3 <$> series)
                               (catMaybes $ t4 <$> series)
 where
  t status =
    Point (created status) (Core.Database.Model.Status.temperature status)
  t2 status =
    Point (created status) (Core.Database.Model.Status.humidity status)
  t3 status = Point (created status)
    <$> Core.Database.Model.Status.temperature_outside status
  t4 status = Point (created status)
    <$> Core.Database.Model.Status.humidity_outside status
