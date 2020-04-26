{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Stream.Model.LightState where

import           Core.State.Model.State         ( State(..)
                                                , Switch(..)
                                                )
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( ToJSON )

data LightState
  = LightState
    { light1 :: Maybe Bool
    , light2 :: Maybe Bool
    }
  deriving(Show, Eq, ToJSON, Generic)

fromState :: State -> LightState
fromState (State l1 l2 _) = LightState (getBool <$> l1) (getBool <$> l2)
 where
  getBool (Manual     b) = b
  getBool (Controlled b) = b
