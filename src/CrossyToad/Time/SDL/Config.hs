{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Time.SDL.Config where

import Control.Lens
import Data.IORef

import CrossyToad.Time.SDL.TimeState

data Config = Config
  { __timeStateRef :: IORef TimeState
  }

makeClassy ''Config

instance HasTimeStateIORef Config where
  timeStateRef = _timeStateRef

initialize :: IO Config
initialize = Config <$> (newIORef initialTimeState)
