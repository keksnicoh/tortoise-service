{-# LANGUAGE OverloadedStrings #-}

module Stream.Model.Action where

import           Stream.Model.LightState        ( LightState(..) )
import           Data.Aeson

newtype Action = LightAction LightState
  deriving(Show, Eq)

instance ToJSON Action where
  toJSON (LightAction lightState) =
    object ["action" .= String "light", "data" .= toJSON lightState]
