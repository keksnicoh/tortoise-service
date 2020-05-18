{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Automation.Service.ProposeSwitchLightServiceSpec where

import           Test.Hspec
import           TestUtil
import           Control.Monad.Reader           ( liftIO
                                                , runReaderT
                                                , ReaderT
                                                )
import qualified Data.Time                     as T

import qualified Dependencies                  as D
import qualified Core.State.Model.State        as CSMState

import           Automation.Service.ProposeSwitchLightService
import           Automation.Free.SimpleController
import           Automation.Header
import           Data.IORef                     ( modifyIORef
                                                , newIORef
                                                , readIORef
                                                )
import           Control.Monad                  ( forM_ )

type ST = ReaderT ProposeLightSwitchEnv IO
newtype ProposeLightSwitchEnv = ProposeLightSwitchEnv T.UTCTime

instance D.HasCurrentTime ProposeLightSwitchEnv ST where
  getCurrentTime (ProposeLightSwitchEnv t) = return t

initialState :: CSMState.State
initialState = CSMState.initialState { CSMState.light1           = Nothing
                                     , CSMState.light2           = Nothing
                                     , CSMState.controlLockDate1 = Nothing
                                     , CSMState.controlLockDate2 = Nothing
                                     }

spec :: Spec
spec = do

  describe "mkProposeLightSwitch" $ do
    let
      date = read "2019-02-03 13:37:42"
      env  = ProposeLightSwitchEnv date
      runService lightStatus = do
        ioRef <- liftIO $ newIORef []
        let getLightStatus :: GetLightStatus ST
            getLightStatus LightId1 = return lightStatus
            getLightStatus LightId2 = return lightStatus

            updateState state = liftIO $ modifyIORef ioRef $ \s -> s ++ [state]
            service = mkProposeSwitchLight getLightStatus updateState

        runReaderT (service LightId1 True) env >>?= ()
        runReaderT (service LightId1 False) env >>?= ()
        runReaderT (service LightId2 True) env >>?= ()
        runReaderT (service LightId2 False) env >>?= ()

        result <- readIORef ioRef
        let mappedStates = (\f -> f initialState) <$> result

        return mappedStates

    forM_
        [ ("LightUndefined", LightUndefined)
        , ("LightOn"       , LightOn)
        , ("LightOff"      , LightOff)
        ]
      $ \(label, lightStatus) ->
          it ("overwrite state by proposed values for: " ++ label) $ do
            runService lightStatus
              >>?= [ initialState
                     { CSMState.light1 = Just (CSMState.Controlled True)
                     , CSMState.controlLockDate1 = Just date
                     }
                   , initialState
                     { CSMState.light1 = Just (CSMState.Controlled False)
                     , CSMState.controlLockDate1 = Just date
                     }
                   , initialState
                     { CSMState.light2 = Just (CSMState.Controlled True)
                     , CSMState.controlLockDate2 = Just date
                     }
                   , initialState
                     { CSMState.light2 = Just (CSMState.Controlled False)
                     , CSMState.controlLockDate2 = Just date
                     }
                   ]

    forM_ [("LightLocked", LightLocked), ("LightManual", LightManual)]
      $ \(label, lightStatus) -> it ("ignore proposition for: " ++ label) $ do
          runService lightStatus >>?= []
