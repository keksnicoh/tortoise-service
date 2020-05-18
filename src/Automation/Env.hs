{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Automation.Env where

import           Data.IORef                     ( IORef )
import           Control.Monad.Reader           ( MonadIO
                                                , liftIO
                                                , ReaderT
                                                )
import           Database.PostgreSQL.Simple     ( Connection )

import qualified Core.Database.Env             as CDEnv
                                                ( HasDbConnection(..) )
import qualified Core.State.Env                as CSEnv
                                                ( HasState(..) )
import qualified Core.State.Model.State        as CSMState
import qualified Dependencies                  as D
import           Automation.Model.HouseStateConfig
import           Automation.Model.SimpleHandlerConfig
import qualified Env                           as E
import qualified Data.Time                     as T
import           Automation.FSM.HouseT          ( HouseT )
import           Automation.FSM.Transitions

type HRT m = HouseT (ReaderT (AutomationEnvironment m) m)

fromMainEnv :: MonadIO m2 => E.Env m -> AutomationEnvironment m2
fromMainEnv env = AutomationEnvironment
  { currentTime         = liftIO T.getCurrentTime
  , houseStateConfig    = E.houseStateConfig env
  , dbConnection        = E.dbConnection env
  , state               = E.state env
  , fsmNRetry           = 10
  , simpleHandlerConfig = E.simpleHandlerConfig env
  }

data AutomationEnvironment m
  = AutomationEnvironment
  { currentTime :: HRT m T.UTCTime
  , houseStateConfig :: HouseStateConfig
  , dbConnection :: Connection
  , state :: IORef CSMState.State
  , fsmNRetry :: Int
  , simpleHandlerConfig :: SimpleHandlerConfig
  }

instance D.HasCurrentTime (AutomationEnvironment IO) (HRT IO) where
  getCurrentTime = currentTime

instance CSEnv.HasState (AutomationEnvironment m) where
  getState = state

instance CDEnv.HasDbConnection (AutomationEnvironment m) where
  getDbConnection = dbConnection

instance HasFSMNRetry (AutomationEnvironment m) where
  getFSMNRetry = fsmNRetry

instance HasHouseStateConfig (AutomationEnvironment m) where
  getHouseStateConfig = houseStateConfig

instance HasSimpleHandlerConfig (AutomationEnvironment m) where
  getSimpleHandlerConfig = simpleHandlerConfig
