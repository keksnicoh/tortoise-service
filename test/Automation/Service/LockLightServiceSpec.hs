{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Automation.Service.LockLightServiceSpec where

import           Test.Hspec

import           Control.Monad.Reader           ( runReaderT )
import           Control.Monad.Writer           ( runWriter
                                                , tell
                                                , Writer
                                                )
import qualified Data.Time                     as T
import qualified Core.State.Model.State        as CSMState
import           Automation.Service.LockLightService
import           Automation.Free.SimpleController
import           Control.Monad.Trans            ( MonadTrans(lift) )
import           OpenEnv

spec :: Spec
spec = do
  describe "mkLockLight" $ do
    it
        "lock the lights properly by id using current time and lock duration from environment"
      $ do
          let date        = read "2018-03-05 13:37:42Z" :: T.UTCTime
              env         = return @(Writer [CSMState.State -> CSMState.State]) date #: nil
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
