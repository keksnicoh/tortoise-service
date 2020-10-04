{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Content.Model.Switch
  ( Switch(..)
  , SwitchRequest(..)
  , toCoreSwitch
  , fromCoreSwitch
  )
where

import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                )
import           GHC.Generics                   ( Generic )
import qualified Core.State.Model.State        as CSM

data Switch
  = Switch
    { value :: Bool
    , controlled :: Bool
    }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data SwitchRequest
  = SwitchRequest
    { light1 :: Maybe Switch
    , light2 :: Maybe Switch
    }
  deriving (Show, Eq, Generic, FromJSON)

toCoreSwitch :: Switch -> CSM.Switch Bool
toCoreSwitch Switch { controlled = False, value = value } = CSM.Manual value
toCoreSwitch Switch { controlled = True, value = value }  = CSM.Controlled value

fromCoreSwitch :: CSM.Switch Bool -> Switch
fromCoreSwitch (CSM.Manual value) =
  Switch { controlled = False, value = value }
fromCoreSwitch (CSM.Controlled value) =
  Switch { controlled = True, value = value }
