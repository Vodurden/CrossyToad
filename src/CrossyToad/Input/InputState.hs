{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Input.InputState where

import Control.Lens
import Data.IORef

import CrossyToad.Input.KeyboardState
import CrossyToad.Input.InputEvent

data InputState = InputState
  { __keyboardState :: KeyboardState
  , _inputEvents :: [InputEvent]
  } deriving (Eq, Show)

makeClassy ''InputState

class HasInputStateIORef a where
  inputStateRef :: Getter a (IORef InputState)

instance HasInputStateIORef (IORef InputState) where
  inputStateRef = id

initialInputState :: InputState
initialInputState = InputState
  { __keyboardState = initialKeyboardState
  , _inputEvents = []
  }

instance HasKeyboardState InputState where
  keyboardState = _keyboardState
