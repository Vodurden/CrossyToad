module CrossyToad.Scene.Game.IntentSpec where

import Test.Tasty.Hspec

import Control.Lens

import CrossyToad.Input.Input
import CrossyToad.Physics.Physics
import CrossyToad.Scene.Game.Intent

mkInputPressed :: Key -> InputState
mkInputPressed key = initialInputState
  & (keyboardState %~ pressKey key)
  . (inputEvents .~ [KeyPressed key])

mkInputHeld :: Key -> InputState
mkInputHeld = mkInputPressed

spec_Scene_Game_Intent :: Spec
spec_Scene_Game_Intent =
  describe "fromInputState" $ do
    it "should Exit when escape is pressed" $ do
      fromInputState (mkInputPressed Escape) `shouldBe` [Exit]

    it "should Move North if W is held" $ do
      fromInputState (mkInputHeld W) `shouldBe` [Move North]

    it "should Move West if A is held" $ do
      fromInputState (mkInputHeld A) `shouldBe` [Move West]

    it "should Move South if S is held" $ do
      fromInputState (mkInputHeld S) `shouldBe` [Move South]

    it "should Move East if D is held" $ do
      fromInputState (mkInputHeld D) `shouldBe` [Move East]
