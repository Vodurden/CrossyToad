{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Vars where

import Control.Lens

import           CrossyToad.Engine.InputState
import           CrossyToad.Scene.Scene (Scene)
import qualified CrossyToad.Scene.Scene as Scene

data Vars = Vars
  { _input :: InputState
  , _scene :: Scene
  }

makeClassy ''Vars

initialVars :: Vars
initialVars = Vars
  { _input = initialInputState
  , _scene = Scene.Title
  }
