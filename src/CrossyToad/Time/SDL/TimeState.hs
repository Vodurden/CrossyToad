{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Time.SDL.TimeState where

import CrossyToad.Time.Seconds

import Control.Lens

data TimeState = TimeState
  { _previousTimestep :: Seconds
  , _currentTimestep :: Seconds
  } deriving (Eq, Show)

makeClassy ''TimeState

initialTimeState :: TimeState
initialTimeState = TimeState
  { _previousTimestep = 0
  , _currentTimestep = 0
  }
