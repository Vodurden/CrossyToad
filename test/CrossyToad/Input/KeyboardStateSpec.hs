module CrossyToad.Input.KeyboardStateSpec where

import Test.Tasty.Hspec

import qualified Data.Set as Set

import CrossyToad.Input.KeyboardState
import CrossyToad.Input.Key

spec_Input_KeyboardState :: Spec
spec_Input_KeyboardState =
  describe "keyPressed" $ do
    it "should be true if the key is pressed" $ do
      keyPressed (KeyboardState (Set.singleton Return)) Return `shouldBe` True

    it "should be true if the key is one of many pressed" $ do
      keyPressed (KeyboardState (Set.fromList [Return, W])) W `shouldBe` True

    it "should be false if the key is not pressed" $ do
      keyPressed (KeyboardState Set.empty) Return `shouldBe` False
