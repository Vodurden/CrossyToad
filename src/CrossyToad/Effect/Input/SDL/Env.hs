{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Input.SDL.Env where

import Control.Lens
import Data.IORef

import CrossyToad.Effect.Input.InputState

data Env = Env
  { __inputStateRef :: IORef InputState
  }

makeClassy ''Env

instance HasInputStateIORef Env where
  inputStateRef = _inputStateRef

initialize :: IO Env
initialize = Env <$> (newIORef initialInputState)
