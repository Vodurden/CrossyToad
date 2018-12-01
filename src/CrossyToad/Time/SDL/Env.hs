{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Time.SDL.Env where

import Control.Lens
import Data.IORef

import CrossyToad.Time.SDL.TimeState

data Env = Env
  { __timeStateRef :: IORef TimeState
  }

makeClassy ''Env

instance HasTimeStateIORef Env where
  timeStateRef = _timeStateRef

initialize :: IO Env
initialize = Env <$> (newIORef initialTimeState)
