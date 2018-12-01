{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.SceneState where

import Control.Lens
import Data.IORef

import CrossyToad.Scene.Game.Game (GameState, HasGameState(..), initialGameState)
import CrossyToad.Scene.Internal (Scene, HasScene(..))
import qualified CrossyToad.Scene.Internal as Scene

data SceneState = SceneState
  { __scene :: Scene
  , __gameState :: GameState
  }

makeClassy ''SceneState

instance HasScene SceneState where
  scene = _scene

instance HasGameState SceneState where
  gameState = _gameState

class HasSceneStateIORef a where
  sceneStateRef :: Getter a (IORef SceneState)

instance HasSceneStateIORef (IORef SceneState) where
  sceneStateRef = id

initialSceneState :: SceneState
initialSceneState = SceneState
  { __scene = Scene.Title
  , __gameState = initialGameState
  }
