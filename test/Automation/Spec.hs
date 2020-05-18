module Automation.Spec where

import           Test.Hspec

import qualified Automation.Model.HouseStateSpec

import qualified Automation.Service.ReadSensorServiceSpec
import qualified Automation.Service.EmergencyServiceSpec
import qualified Automation.Service.GetLightStatusServiceSpec
import qualified Automation.Service.ProposeSwitchLightServiceSpec
import qualified Automation.Service.LockLightServiceSpec

spec :: Spec
spec = do
  describe "Automation.Model.HouseStateSpec"
           Automation.Model.HouseStateSpec.spec

  describe "Automation.Service.ReadSensorServiceSpec"
           Automation.Service.ReadSensorServiceSpec.spec
  describe "Automation.Service.EmergencyServiceSpec"
           Automation.Service.EmergencyServiceSpec.spec
  describe "Automation.Service.GetLightStatusServiceSpec"
           Automation.Service.GetLightStatusServiceSpec.spec
  describe "Automation.Service.ProposeSwitchLightServiceSpec"
           Automation.Service.ProposeSwitchLightServiceSpec.spec
  describe "Automation.Service.LockLightServiceSpec"
           Automation.Service.LockLightServiceSpec.spec


