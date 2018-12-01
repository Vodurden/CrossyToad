module CrossyToad.Scene.Title.IntentSpec where

import Test.Tasty.Hspec

import CrossyToad.Effect.Input.Input
import CrossyToad.Scene.Title.Intent

spec_Scene_Title_Intent :: Spec
spec_Scene_Title_Intent =
  describe "fromInput" $ do
    it "should be StartGame when enter is pressed" $ do
      fromInput [KeyPressed Return] `shouldBe` [StartGame]

    it "should be Quit when escape is pressed" $ do
      fromInput [KeyPressed Escape] `shouldBe` [Quit]
