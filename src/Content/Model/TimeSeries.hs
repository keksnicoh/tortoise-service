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
    { temperature :: [Point T.UTCTime Temperature]
    , humidity :: [Point T.UTCTime Humidity]
    , temperatureOutside :: [Point T.UTCTime Temperature]
    , humidityOutside :: [Point T.UTCTime Humidity]
    } deriving(Generic, Eq, Show, ToJSON)

time = read "2019-02-01 13:37:42"

instance ToSample TimeSeries where
  toSamples _ = singleSample
    $ TimeSeries [Point time 1] [Point time 1] [Point time 1] [Point time 1]

from :: T.UTCTime -> [Status] -> TimeSeries
from start series = TimeSeries
  (seriesOf Core.Database.Model.Status.temperature)
  (seriesOf Core.Database.Model.Status.humidity)
  (seriesOf Core.Database.Model.Status.temperature_outside)
  (seriesOf Core.Database.Model.Status.humidity_outside)
 where
  seriesOf member =
    catMaybes $ (\status -> Point (created status) <$> member status) <$> series
