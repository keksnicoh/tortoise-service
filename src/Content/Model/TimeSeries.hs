{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Content.Model.TimeSeries
  ( TimeSeries(..)
  , Point(..)
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
import           Core.Database.Model.Status    as CDMS
                                                ( Status(..) )
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

from :: T.UTCTime -> T.NominalDiffTime -> [Status] -> TimeSeries
from start dt series = TimeSeries
  (groupDt start dt $ reverse $ seriesOf CDMS.temperature)
  (groupDt start dt $ reverse $ seriesOf CDMS.humidity)
  (groupDt start dt $ reverse $ seriesOf CDMS.temperatureOutside)
  (groupDt start dt $ reverse $ seriesOf CDMS.humidityOutside)
 where
  seriesOf member =
    catMaybes $ (\status -> Point (created status) <$> member status) <$> series

  groupDt
    :: (Fractional a)
    => T.UTCTime
    -> T.NominalDiffTime
    -> [Point T.UTCTime a]
    -> [Point T.UTCTime a]
  groupDt t0 dt []   = []
  groupDt t0 dt list = run t0 [] list
   where
    run t []       []       = []
    run t (b : bs) []       = [safeMean t b bs]
    run t bL       (p : ps) = if T.diffUTCTime (x p) t > dt
      then case bL of
        []       -> run (x p) [p] ps
        (b : bs) -> safeMean t b bs : run (T.addUTCTime dt t) [] (p : ps)
      else run t (bL <> [p]) ps
    safeMean t p ps =
      Point (x p) ((y p + sum (y <$> ps)) / fromIntegral (1 + length ps))
