module CrossyFrog.State where

import CrossyFrog.Engine.InputState

data Vars = Vars
  { vInput :: InputState
  }

initialVars :: Vars
initialVars = Vars
  { vInput = initialInputState
  }
