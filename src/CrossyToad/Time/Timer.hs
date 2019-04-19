{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module CrossyToad.Time.Timer where

import Control.Lens
import Control.Monad.State.Strict.Extended (State, execState, modify')

import CrossyToad.Time.Seconds (Seconds)

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
loop :: Timer -> Timer
loop t | running t = t
       | otherwise = start t

running :: Timer -> Bool
running timer' = timer' ^. currentTime > 0

finished :: Timer -> Bool
finished = not . running

-- | Ticks the timer by the given delta
-- |
-- | Returns any unused delta time
tick :: (HasTimer ent) => Seconds -> State ent Seconds
tick delta = do
  timer' <- use timer
  let nextSeconds = (timer' ^. currentTime) - delta
  let remainingDelta = if nextSeconds < 0 then abs nextSeconds else 0
  timer.currentTime .= (max 0 nextSeconds)
  pure remainingDelta

tickL
  :: (HasTimer timer)
  => Lens' ent timer      -- ^ The timer to tick
  -> Seconds              -- ^ Amount of time to tick ahead by
  -> State ent b          -- ^ Update to apply if the timer has not finished
  -> State ent b          -- ^ Update to apply if the timer has finished
  -> State ent b
tickL timerL delta onNoTick onTick = do
    timer' <- use (timerL.timer)
    let nextTimer = tickTimer delta timer'
    timerL . timer .= nextTimer
    if finished nextTimer
      then onTick
      else onNoTick
  where
    tickTimer :: Seconds -> Timer -> Timer
    tickTimer delta' timer' =
      let nextSeconds = (timer' ^. currentTime) - delta'
      in timer' & (currentTime .~ max 0 nextSeconds)

tickEnt :: (HasTimer ent) => Seconds -> (ent -> ent) -> ent -> ent
tickEnt delta onTick =
  execState $ tickL timer delta (modify' id) (modify' onTick)

tickOver :: (HasTimer timer) => Lens' ent timer -> Seconds -> (ent -> ent) -> ent -> ent
tickOver timerL delta onTick =
  execState $ tickL timerL delta (modify' id) (modify' onTick)
