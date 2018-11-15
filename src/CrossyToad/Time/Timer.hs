{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Time.Timer where

import Control.Lens
import Control.Monad.State (MonadState)

import CrossyToad.Time.Seconds
import CrossyToad.Time.Time

data Timer = Timer
  { _startTime :: Seconds      -- ^ The time to start at when reset
  , _currentTime :: Seconds    -- ^ The current time. Counts down to 0
  } deriving (Eq, Show)

makeClassy ''Timer

mk :: Seconds -> Timer
mk startTime' = Timer
  { _startTime = startTime'
  , _currentTime = 0
  }

start :: Timer -> Timer
start timer' = timer' & currentTime .~ (timer' ^. startTime)

running :: Timer -> Bool
running timer' = timer' ^. currentTime > 0

-- | Steps a Timers and returns any remaining delta time (if any)
stepEff :: (Time m, MonadState s m, HasTimer s) => m Seconds
stepEff = do
  delta <- deltaTime
  step delta

step :: (MonadState s m, HasTimer s) => Seconds -> m Seconds
step delta = do
  timer' <- use timer
  let nextSeconds = (timer' ^. currentTime) - delta
  let remainingDelta = if nextSeconds < 0 then abs nextSeconds else 0
  timer.currentTime .= (max 0 nextSeconds)
  pure remainingDelta