{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Engine.InputState where

import Control.Lens

import CrossyToad.Engine.KeyState

data InputState = InputState
  { _enter :: KeyState
  , _esc :: KeyState
  , _quit :: Bool
  } deriving (Show, Eq)

initialInputState :: InputState
initialInputState = InputState
  { _enter = Released
  , _esc = Released
  , _quit = False
  }

makeClassy ''InputState