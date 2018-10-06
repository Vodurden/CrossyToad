module CrossyToad.Engine.InputState where

data InputState = InputState
  { quit :: Bool
  } deriving (Show, Eq)

initialInputState :: InputState
initialInputState = InputState
  { quit = False
  }
