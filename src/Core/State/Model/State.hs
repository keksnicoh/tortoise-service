module Core.State.Model.State
  ( initialState
  , State(..)
  , Switch(..)
  , HouseMonitor(..)
  )
where

import           Data.Time

data Switch a
  = Manual a
  | Controlled a
  deriving (Show, Eq)

data HouseMonitor
  = MonitorOK
  | MonitorIdle
  | MonitorEmergency
  | MonitorOff
  deriving (Eq, Show)

data State
  = State
    { light1 :: Maybe (Switch Bool)
    , light2 :: Maybe (Switch Bool)
    , webcamDate :: Maybe UTCTime
    , webcamRequest :: Maybe UTCTime
    , houseMonitor :: HouseMonitor
    }
  deriving (Show, Eq)

initialState :: State
initialState = State Nothing Nothing Nothing Nothing MonitorOff
