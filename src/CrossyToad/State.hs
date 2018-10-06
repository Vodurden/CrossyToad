module CrossyToad.State where

import           CrossyToad.Engine.InputState
import           CrossyToad.Scene.Scene (Scene)
import qualified CrossyToad.Scene.Scene as Scene

data Vars = Vars
  { vInput :: InputState
  , vScene :: Scene
  }

initialVars :: Vars
initialVars = Vars
  { vInput = initialInputState
  , vScene = Scene.Title
  }
