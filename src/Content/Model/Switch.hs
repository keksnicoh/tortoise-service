{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Content.Model.Switch
  ( Switch(..)
  , SwitchRequest(..)
  , toStateSwitch
  , fromStateSwitch
  )
where

import           Data.Aeson
import           GHC.Generics                   ( Generic )
import           Data.Swagger
import qualified Core.State.Model.State        as CSM

data Switch
  = Switch
    { value :: Bool
    , controlled :: Bool
    }
  deriving (Show, Eq, Generic, ToSchema, ToJSON, FromJSON)

data SwitchRequest
  = SwitchRequest
    { light1 :: Maybe Switch
    , light2 :: Maybe Switch
    }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

toStateSwitch :: Switch -> CSM.Switch Bool
toStateSwitch Switch { controlled = False, value = value } = CSM.Manual value
toStateSwitch Switch { controlled = True, value = value } =
  CSM.Controlled value

fromStateSwitch :: CSM.Switch Bool -> Switch
fromStateSwitch (CSM.Manual value) =
  Switch { controlled = False, value = value }
fromStateSwitch (CSM.Controlled value) =
  Switch { controlled = True, value = value }
