module CrossyToad.Scene.Title.TitleSpec where

import           Control.Monad.State (execStateT)
import           Data.Functor.Identity (runIdentity)
import           Test.Tasty.Hspec

import qualified CrossyToad.Scene.Scene as Scene
import qualified CrossyToad.Scene.Title.Intent as Intent
import           CrossyToad.Scene.Title.Title

spec_Scene_Title :: Spec
spec_Scene_Title = 
  describe "stepIntent" $ do
    it "should start the game when the intent is StartGame" $
      runIdentity (execStateT (stepIntent Intent.StartGame) Scene.Title) `shouldBe` Scene.Play

    it "should quit the game when the intent is Quit" $
      runIdentity (execStateT (stepIntent Intent.Quit) Scene.Title) `shouldBe` Scene.Quit
