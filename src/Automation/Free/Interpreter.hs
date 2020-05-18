{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Automation.Free.Interpreter where

import           Control.Monad.Reader           ( MonadReader )
import           Control.Monad.Free
import qualified Dependencies                  as D
import qualified Core.Database.Model.Status    as CDMStatus
import           Automation.Header
import           Automation.Free.SimpleController

mkInterpreter
  :: (MonadReader e m, D.HasCurrentTime e m)
  => GetLightStatus m
  -> ProposeSwitchLight m
  -> LockLight m
  -> CDMStatus.FetchStatusRepository m
  -> SimpleControllerInterpreter a m
mkInterpreter getLightStatus proposeSwitchLight lockLight fetchStatusRepository
  = foldFree $ \case
    GetTemperature n -> n <$> do
      result <- fetchStatusRepository
      return $ CDMStatus.temperature =<< safeHead result
    GetLightStatus lightId n       -> n <$> getLightStatus lightId
    ProposeLightSwitch lightId s n -> n <$ proposeSwitchLight lightId s
    LockLight lightId n            -> n <$ lockLight lightId
 where
  safeHead []       = Nothing
  safeHead (x : xs) = Just x
