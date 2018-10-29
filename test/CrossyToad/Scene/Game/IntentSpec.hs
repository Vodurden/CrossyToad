module CrossyToad.Scene.Game.IntentSpec where

import Control.Lens
import Test.Tasty.Hspec

import CrossyToad.Input.Input
import CrossyToad.Scene.Game.Intent

spec_Scene_Game_Intent :: Spec
spec_Scene_Game_Intent =
  describe "fromInput" $ do
    it "should be Exit when escape is pressed" $ do
      let input = (set esc Pressed) initialInputState
      fromInput input `shouldBe` [Exit]
