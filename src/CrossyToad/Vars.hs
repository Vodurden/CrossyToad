{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Vars where

import Control.Lens

import           CrossyToad.Engine.InputState
import           CrossyToad.Scene.Scene (Scene, HasScene)
import qualified CrossyToad.Scene.Scene as Scene

data Vars = Vars
  { __inputState :: InputState
  , __scene :: Scene
  }

makeClassy ''Vars

instance HasInputState Vars where
  inputState = _inputState

instance HasScene Vars where
  scene = _scene

initialVars :: Vars
initialVars = Vars
  { __inputState = initialInputState
  , __scene = Scene.Title
  }
