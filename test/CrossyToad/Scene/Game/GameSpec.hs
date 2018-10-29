module CrossyToad.Scene.Game.GameSpec where

import           Control.Monad.State (execStateT)
import           Data.Functor.Identity (runIdentity)
import           Test.Tasty.Hspec

import qualified CrossyToad.Scene.Scene as Scene
import qualified CrossyToad.Scene.Game.Intent as Intent
import           CrossyToad.Scene.Game.Game

spec_Scene_Game :: Spec
spec_Scene_Game =
  describe "stepIntent" $ do
    it "should return to the title when the intent is Exit" $
      runIdentity (execStateT (stepIntent Intent.Exit) Scene.Game) `shouldBe` Scene.Title
