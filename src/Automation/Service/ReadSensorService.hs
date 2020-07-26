{-# LANGUAGE FlexibleContexts #-}

module Automation.Service.ReadSensorService where

import           Control.Monad.Reader           ( MonadReader )
import           OpenEnv
import           Automation.FSM.HouseFSM
import qualified Core.Database.Model.Status    as CDMStatus
import qualified Data.Time                     as T
import           Data.Maybe                     ( mapMaybe )
import           Automation.Model.HouseStateConfig

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
