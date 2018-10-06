module CrossyToad.State where

import CrossyToad.Engine.InputState

data Vars = Vars
  { vInput :: InputState
  }

initialVars :: Vars
initialVars = Vars
  { vInput = initialInputState
  }
