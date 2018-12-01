{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Vars where

import Control.Lens

import CrossyToad.Input.Input

data Vars = Vars
  { __inputState :: InputState
  }

makeClassy ''Vars

instance HasInputState Vars where
  inputState = _inputState

initialVars :: Vars
initialVars = Vars
  { __inputState = initialInputState
  }
