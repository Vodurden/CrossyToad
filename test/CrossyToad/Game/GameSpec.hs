{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.GameSpec where

-- import           Control.Monad.State (State, execState)
-- import           Control.Lens

-- import           Test.Tasty.Hspec

-- import           CrossyToad.Scene.Scene (Scene, HasScene(..))
-- import qualified CrossyToad.Scene.Scene as Scene
-- import qualified CrossyToad.Game.Intent as Intent
-- import           CrossyToad.Game.Game

-- data MockState = MockState
--   { __scene :: Scene
--   , __gameState :: GameState
--   } deriving (Eq, Show)

-- makeClassy ''MockState

-- instance HasScene MockState where scene = _scene
-- instance HasGameState MockState where gameState = _gameState

-- initialMockState :: MockState
-- initialMockState = MockState
--   { __scene = Scene.Game
--   , __gameState = initialGameState
--   }

-- runMock :: State MockState a -> MockState
-- runMock f = execState f initialMockState

-- spec_Scene_Game :: Spec
-- spec_Scene_Game =
--   describe "stepIntent" $ do
--     it "should return to the title when the intent is Exit" $ do
--       let result = (stepIntent Intent.Exit initialMockState)
--       result ^. scene `shouldBe` Scene.Title
