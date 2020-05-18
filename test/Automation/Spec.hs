module Automation.Spec where

import           Test.Hspec
import qualified Automation.Model.HouseStateSpec
import qualified Automation.Service.ReadSensorServiceSpec
import qualified Automation.Service.EmergencyServiceSpec

spec :: Spec
spec = do
  describe "Automation.Model.HouseStateSpec"
           Automation.Model.HouseStateSpec.spec
  describe "Automation.Service.ReadSensorServiceSpec"
           Automation.Service.ReadSensorServiceSpec.spec
  describe "Automation.Service.EmergencyServiceSpec"
           Automation.Service.EmergencyServiceSpec.spec

