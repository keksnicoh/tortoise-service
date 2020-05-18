{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Automation.Model.HouseStateSpec where

import           Test.Hspec
import           Control.Monad.Reader           ( forM_ )
import           Automation.Model.HouseState
import           Automation.FSM.HouseFSM

spec :: Spec
spec = do
  describe "HouseState#Show"
    $ forM_
        [ (show Initializing              , "Initializing")
        , (show (HasSensorData $ Bound 12), "HasSensorData Bound 12.0")
        , (show TemperatureBound          , "TemperatureBound")
        , (show RetrySensor               , "RetrySensor")
        , (show Emergency                 , "Emergency")
        , ( show (Terminating OutOfControlReason)
          , "Terminating OutOfControlReason"
          )
        ]
    $ \(subj, expected) -> it ("render " ++ expected) $ subj `shouldBe` expected
