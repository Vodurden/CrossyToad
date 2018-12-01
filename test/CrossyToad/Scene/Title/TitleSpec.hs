module CrossyToad.Scene.Title.TitleSpec where

import           Test.Tasty.Hspec

import qualified CrossyToad.Scene.Scene as Scene
import qualified CrossyToad.Scene.Title.Intent as Intent
import           CrossyToad.Scene.Title.Title

spec_Scene_Title :: Spec
spec_Scene_Title =
  describe "stepIntent" $ do
    it "should start the game when the intent is StartGame" $
      stepIntent Intent.StartGame Scene.Title `shouldBe` Scene.Game

    it "should quit the game when the intent is Quit" $
      stepIntent Intent.Quit Scene.Title `shouldBe` Scene.Quit
