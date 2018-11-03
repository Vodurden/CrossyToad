{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Vars where

import Control.Lens

import           CrossyToad.Scene.Scene (Scene, HasScene)
import qualified CrossyToad.Scene.Scene as Scene
import           CrossyToad.Scene.Game.Game (GameState, HasGameState(..), initialGameState)

data Vars = Vars
  { __scene :: Scene
  , __gameState :: GameState
  }

makeClassy ''Vars

instance HasScene Vars where
  scene = _scene

instance HasGameState Vars where
  gameState = _gameState

initialVars :: Vars
initialVars = Vars
  { __scene = Scene.Title
  , __gameState = initialGameState
  }
