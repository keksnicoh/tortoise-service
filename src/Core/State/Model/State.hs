module Core.State.Model.State
  ( initialState
  , State(..)
  , Switch(..)
  )
where

import Data.Time

data Switch a
  = Manual a
  | Controlled a
  deriving (Show, Eq)

data State
  = State
    { light1 :: Maybe (Switch Bool)
    , light2 :: Maybe (Switch Bool)
    , webcamDate :: Maybe UTCTime
    }
  deriving (Show, Eq)

initialState :: State
initialState = State Nothing Nothing Nothing
