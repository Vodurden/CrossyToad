{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Time.MonadTime.SDL.MonadTimeState where

import CrossyToad.Time.Seconds

import Control.Lens
import Data.IORef

data MonadTimeState = MonadTimeState
  { _previousTime :: Seconds
  , _currentTime :: Seconds
  } deriving (Eq, Show)

makeClassy ''MonadTimeState

class HasMonadTimeStateIORef a where
  monadTimeStateRef :: Getter a (IORef MonadTimeState)

instance HasMonadTimeStateIORef (IORef MonadTimeState) where
  monadTimeStateRef = getting id

initialMonadTimeState :: MonadTimeState
initialMonadTimeState = MonadTimeState
  { _previousTime = 0
  , _currentTime = 0
  }

deltaTime :: MonadTimeState -> Seconds
deltaTime ts = (ts^.currentTime) - (ts^.previousTime)
