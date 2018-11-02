{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Input.InputState where

import Control.Lens

import CrossyToad.Input.KeyState

data InputState = InputState
  { _enter :: KeyState
  , _esc :: KeyState
  , _w :: KeyState
  , _a :: KeyState
  , _s :: KeyState
  , _d :: KeyState
  , _quit :: Bool
  } deriving (Show, Eq)

initialInputState :: InputState
initialInputState = InputState
  { _enter = Released
  , _esc = Released
  , _w = Released
  , _a = Released
  , _s = Released
  , _d = Released
  , _quit = False
  }

makeClassy ''InputState
