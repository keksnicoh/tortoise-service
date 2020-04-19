{-# LANGUAGE DeriveGeneric #-}

module Content.Model.SwitchRequest where
import           GHC.Generics                   ( Generic )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )

data SwitchRequest
  = SwitchRequest
    { light1 :: Maybe Bool
    , light2 :: Maybe Bool
    }
  deriving (Show, Eq, Generic)

instance ToJSON SwitchRequest
instance FromJSON SwitchRequest
