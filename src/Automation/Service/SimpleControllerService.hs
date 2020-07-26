{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Automation.Service.SimpleControllerService where

import           Automation.Header
import           Control.Monad.Reader           ( MonadReader )
import           OpenEnv
import           Control.Monad                  ( when )
import           Automation.Model.SimpleHandlerConfig
import           Automation.Free.SimpleController

mkControllerHandler
  :: (MonadReader e m, Provides SimpleHandlerConfig e)
  => SimpleControllerInterpreter () m
  -> ControlHandler m
mkControllerHandler interpreter = program <$> provide >>= interpreter
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
      proposeLightSwitch lightId (temperature < highT tRange)
    lightA _ _ _ _ = return ()

    switch lightId value = do
      proposeLightSwitch lightId value
      lockLight lightId
