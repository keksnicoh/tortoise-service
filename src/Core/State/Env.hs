{-# LANGUAGE FlexibleInstances #-}
module Core.State.Env where

import           Core.State.Model.State
import GHC.IORef (IORef)

class HasState a where getState :: a -> IORef State
instance HasState (IORef State) where
  getState = id
