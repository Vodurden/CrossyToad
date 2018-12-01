{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Time.Timer where

import Control.Lens
import Control.Monad.State.Extended (State, execState)

import CrossyToad.Effect.Time.Seconds
import CrossyToad.Effect.Time.Time

data Timer = Timer
  { _startTime :: !Seconds      -- ^ The time to start at when reset
  , _currentTime :: !Seconds    -- ^ The current time. Counts down to 0
  } deriving (Eq, Show)

makeClassy ''Timer

mk :: Seconds -> Timer
mk startTime' = Timer
  { _startTime = startTime'
  , _currentTime = 0
  }

start :: Timer -> Timer
start timer' = timer' & currentTime .~ (timer' ^. startTime)

-- | Start the timer if isn't running
restart :: Timer -> Timer
restart t | running t = t
          | otherwise = start t

running :: Timer -> Bool
running timer' = timer' ^. currentTime > 0

finished :: Timer -> Bool
finished = not . running

-- | Steps a Timer
step :: (Time m, HasTimer ent) => ent -> m ent
step ent = do
  delta <- deltaTime
  pure $ execState (stepBy delta) ent

-- | Steps the timer by the given delta
-- |
-- | Returns any unused delta time
stepBy :: (HasTimer ent) => Seconds -> State ent Seconds
stepBy delta = do
  timer' <- use timer
  let nextSeconds = (timer' ^. currentTime) - delta
  let remainingDelta = if nextSeconds < 0 then abs nextSeconds else 0
  timer.currentTime .= (max 0 nextSeconds)
  pure remainingDelta
