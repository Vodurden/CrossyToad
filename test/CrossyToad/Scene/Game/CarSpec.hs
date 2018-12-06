module CrossyToad.Scene.Game.CarSpec where

import           Control.Lens
import           Linear.V2

import           Test.Tasty.Hspec

import qualified CrossyToad.Effect.Renderer.ImageAsset as ImageAsset
import           CrossyToad.Effect.Renderer.RenderCommand
import           CrossyToad.Physics.Physics
import           CrossyToad.Scene.Game.Car as Car

spec_Scene_Game_Car :: Spec
spec_Scene_Game_Car = do
  describe "render" $ do
    it "should draw the Car asset" $ do
      let asset = render (Car.mk (V2 0 0) East) ^? _Draw . _1
      asset `shouldBe` (Just ImageAsset.Car)
