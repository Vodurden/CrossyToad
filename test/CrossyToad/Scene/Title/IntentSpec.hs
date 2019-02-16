module CrossyToad.Title.IntentSpec where

import           Test.Tasty.Hspec

import           CrossyToad.Input.InputEvent (InputEvent(..))
import qualified CrossyToad.Input.Key as Key
import           CrossyToad.Title.Intent

spec_Scene_Title_Intent :: Spec
spec_Scene_Title_Intent =
  describe "fromInput" $ do
    it "should be StartGame when enter is pressed" $ do
      fromInput [KeyPressed Key.Return] `shouldBe` [StartGame]

    it "should be Quit when escape is pressed" $ do
      fromInput [KeyPressed Key.Escape] `shouldBe` [Quit]
