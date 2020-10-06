{-# LANGUAGE FlexibleContexts #-}

module Automation.Service.ReadSensorService where

import           Automation.FSM.HouseFSM           (TemperatureSensor (..))
import           Automation.Model.HouseStateConfig (HouseStateConfig (maxStatusAge, maxTemperature, minTemperature))
import           Control.Monad.Reader              (MonadReader)
import qualified Core.Database.Model.Status        as CDMStatus
import           Data.Maybe                        (mapMaybe)
import qualified Data.Time                         as T
import           OpenEnv                           (Embedded, Provides,
                                                    embedded, provide)

{-| reads the last defined temperature within a configured time range.
    if a temperature value exists then it will be interpreted to be
    High, Low or Bound. If no temperature value is available, the
    result of this function is Nothing. -}
mkReadSensor
  :: (MonadReader e m, Provides HouseStateConfig e, Embedded T.UTCTime e m)
  => CDMStatus.FetchStatusRepository m
  -> m (Maybe TemperatureSensor)
mkReadSensor fetchStatusRepository = do
  minT         <- minTemperature <$> provide
  maxT         <- maxTemperature <$> provide
  maxStatusAge <- maxStatusAge <$> provide
  now          <- embedded
  let ageFilter s = T.diffUTCTime now (CDMStatus.created s) <= maxStatusAge
      interpret [] = Nothing
      interpret (x : _) | x < minT  = Just (Low x)
                        | x > maxT  = Just (High x)
                        | otherwise = Just (Bound x)
      pipeline = interpret . mapMaybe CDMStatus.temperature . filter ageFilter
  pipeline <$> fetchStatusRepository
