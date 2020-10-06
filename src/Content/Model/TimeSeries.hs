{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Content.Model.TimeSeries
    ( TimeSeries(..)
    , Point(..)
    , from
    , group
    , groupTimeSeries) where

import qualified Core.Database.Model.Status as CDMS (Status (..))
import           Core.Internal              (Humidity, Temperature)
import           Data.Aeson                 (ToJSON)
import           Data.Maybe                 (catMaybes)
import qualified Data.Time                  as T
import           GHC.Generics               (Generic)

data Point a b = Point { x :: a, y :: b }
  deriving (Show, Eq, ToJSON, Generic)

data TimeSeries =
  TimeSeries { temperature        :: [Point T.UTCTime Temperature]
             , humidity           :: [Point T.UTCTime Humidity]
             , temperatureOutside :: [Point T.UTCTime Temperature]
             , humidityOutside    :: [Point T.UTCTime Humidity]
             }
  deriving (Generic, Eq, Show, ToJSON)

from :: [CDMS.Status] -> TimeSeries
from series = TimeSeries
  (seriesOf CDMS.temperature)
  (seriesOf CDMS.humidity)
  (seriesOf CDMS.temperatureOutside)
  (seriesOf CDMS.humidityOutside)
  where
    seriesOf member = catMaybes
      $ (\s -> Point (CDMS.created s) <$> member s) <$> series

groupTimeSeries :: T.NominalDiffTime -> TimeSeries -> TimeSeries
groupTimeSeries dt timeSeries =
  timeSeries { temperature = group dt (temperature timeSeries)
             , humidity = group dt (humidity timeSeries)
             , temperatureOutside = group dt (temperatureOutside timeSeries)
             , humidityOutside = group dt (humidityOutside timeSeries)
             }

-- todo specs
group :: (Fractional a)
      => T.NominalDiffTime
      -> [Point T.UTCTime a]
      -> [Point T.UTCTime a]
group dt [] = []
group dt (p:ps) = run (x p) [p] ps []
  where
    run t [] [] !acc = acc
    run t (b:bs) [] !acc = acc ++ [safeMean t b bs]
    run t !bL (p:ps) !acc =
      if T.diffUTCTime t (x p) > dt
      then case bL of
        []     -> run (x p) [p] ps acc
        (b:bs) -> run (T.addUTCTime dt t) [] (p:ps) (acc ++ [safeMean t b bs])
      else run t (bL <> [p]) ps acc

    safeMean t !p !ps =
      Point (x p) ((y p + sum (y <$> ps)) / fromIntegral (1 + length ps))
