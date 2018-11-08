{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Vars where

import Control.Lens

import           CrossyToad.Scene.Scene (Scene, HasScene)
import qualified CrossyToad.Scene.Scene as Scene
import           CrossyToad.Scene.Game.Game (GameState, HasGameState(..), initialGameState)
import qualified CrossyToad.Time.SDL.SDL as SDLTime
import           CrossyToad.Input.Input

data Vars = Vars
  { __scene :: Scene
  , __gameState :: GameState
  , __inputState :: InputState
  , __timeState :: SDLTime.TimeState
  }

makeClassy ''Vars

instance HasScene Vars where
  scene = _scene

instance HasGameState Vars where
  gameState = _gameState

instance HasInputState Vars where
  inputState = _inputState

instance SDLTime.HasTimeState Vars where
  timeState = _timeState

initialVars :: Vars
initialVars = Vars
  { __scene = Scene.Title
  , __gameState = initialGameState
  , __inputState = initialInputState
  , __timeState = SDLTime.initialTimeState
  }
