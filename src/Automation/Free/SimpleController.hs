{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

{-| Free Monad Algebra providing a set of operations to control the light switches
    The choice of using Free for this package is motivated to gain practice 
    with different concepts.

    resources
    - https://markkarpov.com/post/free-monad-considered-harmful.html
-}
module Automation.Free.SimpleController where

import           Core.Internal
import           Control.Monad.Free

type SimpleControllerM = Free SimpleController
type SimpleControllerInterpreter a m = SimpleControllerM a -> m a

data LightId
  = LightId1
  | LightId2
  deriving (Eq, Show)

data LightStatus
  = LightOn
  | LightOff
  | LightLocked
  | LightUndefined
  | LightManual
  deriving (Show, Eq)

data SimpleController a
  = GetLightStatus LightId (LightStatus -> a)
  | ProposeLightSwitch LightId Bool a
  | GetTemperature (Maybe Temperature -> a)
  | LockLight LightId a
  deriving (Functor)

getTemperature :: SimpleControllerM (Maybe Temperature)
getTemperature = Free (GetTemperature return)

getLightStatus :: LightId -> SimpleControllerM LightStatus
getLightStatus lightId = Free (GetLightStatus lightId return)

proposeLightSwitch :: LightId -> Bool -> SimpleControllerM ()
proposeLightSwitch l b = liftF (ProposeLightSwitch l b ())

lockLight :: LightId -> SimpleControllerM ()
lockLight l = liftF (LockLight l ())
