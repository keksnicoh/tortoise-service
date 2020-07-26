{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Automation.Service.LockLightServiceSpec where

import           Test.Hspec

import           Control.Monad.Reader           ( runReaderT
                                                , ReaderT
                                                )
import           Control.Monad.Writer           ( runWriter
                                                , tell
                                                , Writer
                                                )

import qualified Data.Time                     as T
import qualified Dependencies                  as D

import qualified Core.State.Repository.State   as CSRState
import qualified Core.State.Model.State        as CSMState

import           Automation.Service.LockLightService
import           Automation.Free.SimpleController
import           Control.Monad.Trans            ( MonadTrans(lift) )

-- mock environment
type ST
  = ReaderT LockLightServiceEnv (Writer [CSMState.State -> CSMState.State])

newtype LockLightServiceEnv = LockLightServiceEnv T.UTCTime

instance D.HasCurrentTime LockLightServiceEnv ST where
  getCurrentTime (LockLightServiceEnv time) = return time

spec :: Spec
spec = do
  describe "mkLockLight" $ do
    it
        "lock the lights properly by id using current time and lock duration from environment"
      $ do
          let date = read "2018-03-05 13:37:42Z"
              env  = LockLightServiceEnv date
              updateState :: CSRState.UpdateState ST
              updateState = lift . tell . pure
              service     = mkLockLight updateState
              lockLights  = do
                runReaderT (service LightId1) env
                runReaderT (service LightId2) env
              (_, result) = runWriter lockLights
              states      = (\f -> f CSMState.initialState) <$> result
          states
            `shouldBe` [ CSMState.initialState
                         { CSMState.controlLockDate1 =
                           Just (read "2018-03-05 13:37:42Z")
                         }
                       , CSMState.initialState
                         { CSMState.controlLockDate2 =
                           Just (read "2018-03-05 13:37:42Z")
                         }
                       ]
