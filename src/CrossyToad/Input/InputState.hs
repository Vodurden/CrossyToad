{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Input.InputState where

import Control.Lens

import CrossyToad.Input.KeyboardState
import CrossyToad.Input.InputEvent

data InputState = InputState
  { __keyboardState :: KeyboardState
  , _inputEvents :: [InputEvent]
  } deriving (Eq, Show)

makeClassy ''InputState

initialInputState :: InputState
initialInputState = InputState
  { __keyboardState = initialKeyboardState
  , _inputEvents = []
  }

instance HasKeyboardState InputState where
  keyboardState = _keyboardState
