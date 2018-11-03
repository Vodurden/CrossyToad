module CrossyToad.Scene.Game.IntentSpec where

import Test.Tasty.Hspec

import CrossyToad.Input.Input
import CrossyToad.Scene.Game.Intent

spec_Scene_Game_Intent :: Spec
spec_Scene_Game_Intent =
  describe "fromInput" $ do
    it "should be Exit when escape is pressed" $ do
      fromInput [KeyPressed Escape] `shouldBe` [Exit]
