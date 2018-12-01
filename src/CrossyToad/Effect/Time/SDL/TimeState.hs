{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Time.SDL.TimeState where

import CrossyToad.Effect.Time.Seconds

import Control.Lens
import Data.IORef

data TimeState = TimeState
  { _previousTime :: Seconds
  , _currentTime :: Seconds
  } deriving (Eq, Show)

makeClassy ''TimeState

class HasTimeStateIORef a where
  timeStateRef :: Getter a (IORef TimeState)

instance HasTimeStateIORef (IORef TimeState) where
  timeStateRef = getting id

initialTimeState :: TimeState
initialTimeState = TimeState
  { _previousTime = 0
  , _currentTime = 0
  }

deltaTime :: TimeState -> Seconds
deltaTime ts = (ts^.currentTime) - (ts^.previousTime)
