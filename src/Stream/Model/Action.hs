{-# LANGUAGE OverloadedStrings #-}

module Stream.Model.Action where

import           Data.Aeson              (KeyValue ((.=)), ToJSON (toJSON),
                                          Value (String), object)
import           Stream.Model.LightState (LightState (..))

data Action
  = LightAction LightState
  | LightChangedAction LightState
  | PingAction
  | WebcamAction
  deriving(Show, Eq)

instance ToJSON Action where
  toJSON (LightAction lightState) =
    object ["action" .= String "light", "data" .= toJSON lightState]
  toJSON (LightChangedAction lightState) =
    object ["action" .= String "light_changed", "data" .= toJSON lightState]
  toJSON WebcamAction = object ["action" .= String "webcam"]
  toJSON PingAction   = object ["action" .= String "ping"]
