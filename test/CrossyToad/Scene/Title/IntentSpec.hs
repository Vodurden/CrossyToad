module CrossyToad.Scene.Title.IntentSpec where

import Control.Lens
import Test.Tasty.Hspec

import CrossyToad.Engine.InputState
import CrossyToad.Engine.KeyState
import CrossyToad.Scene.Title.Intent

spec_Scene_Title_Intent :: Spec
spec_Scene_Title_Intent =
  describe "fromInput" $ do
    it "should be StartGame when enter is pressed" $ do
      let input = (set enter Pressed) initialInputState
      fromInput input `shouldBe` [StartGame]

    it "should be Quit when escape is pressed" $ do
      let input = (set esc Pressed) initialInputState
      fromInput input `shouldBe` [Quit]
