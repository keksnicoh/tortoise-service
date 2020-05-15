{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Automation.Controller where

import qualified Core.FSM                      as CFSM
                                                ( ControlHandler )

import qualified Dependencies                  as D
import           Control.Monad.Reader           ( reader
                                                , MonadReader
                                                )
import qualified Core.Database.Model.Status    as CDMStatus
import           Control.Monad                  ( when )
import           Automation.Header
import           Automation.Model.SimpleHandlerConfig
import           Core.FreeSimpleController
import           Control.Monad.Free

mkControllerHandler
  :: (MonadReader e m, HasSimpleHandlerConfig e)
  => Interpreter () m
  -> CFSM.ControlHandler m
mkControllerHandler interpreter =
  program <$> reader getSimpleHandlerConfig >>= interpreter
 where
  program config = do
    fmap lightA <$> getTemperature >>= \case
      Nothing           -> return ()
      Just controlLight -> do
        getLightStatus LightId1 >>= controlLight LightId1 (l1TRange config)
        getLightStatus LightId2 >>= controlLight LightId2 (l2TRange config)
   where
    lightA temperature lightId tRange LightOff =
      when (temperature < lowT tRange) $ switch lightId True
    lightA temperature lightId tRange LightOn =
      when (temperature > highT tRange) $ switch lightId False
    lightA temperature lightId tRange LightUndefined =
      proposeLightSwitch lightId (temperature < lowT tRange)
    lightA _ _ _ _ = return ()

    switch lightId value = do
      proposeLightSwitch lightId value
      lockLight lightId

mkInterpreter
  :: (MonadReader e m, D.HasCurrentTime e m)
  => GetLightStatus m
  -> ProposeSwitchLight m
  -> LockLight m
  -> CDMStatus.FetchStatusRepository m
  -> Interpreter a m
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

