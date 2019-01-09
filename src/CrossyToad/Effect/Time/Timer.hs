{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module CrossyToad.Effect.Time.Timer where

import Control.Lens
import Control.Monad.State.Extended (StateT, State, execState, modify', hoistState)

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
loop :: Timer -> Timer
loop t | running t = t
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

tick
  :: (Time m, HasTimer timer)
  => Lens' ent timer      -- ^ The timer to tick
  -> State ent b          -- ^ Update to apply if the timer has not finished
  -> State ent b          -- ^ Update to apply if the timer has finished
  -> StateT ent m b
tick timerL onNoTick onTick = do
  delta <- deltaTime
  hoistState $ tickBy timerL delta onNoTick onTick

tickBy
  :: (HasTimer timer)
  => Lens' ent timer      -- ^ The timer to tick
  -> Seconds              -- ^ Amount of time to tick ahead by
  -> State ent b          -- ^ Update to apply if the timer has not finished
  -> State ent b          -- ^ Update to apply if the timer has finished
  -> State ent b
tickBy timerL delta onNoTick onTick = do
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

tickEnt :: (Time m, HasTimer ent) => (ent -> ent) -> ent -> m ent
tickEnt onTick ent = do
  delta <- deltaTime
  pure $ tickOverBy timer delta onTick ent

tickEntBy :: (HasTimer ent) => Seconds -> (ent -> ent) -> ent -> ent
tickEntBy delta onTick =
  execState $ tickBy timer delta (modify' id) (modify' onTick)

tickOver :: (Time m, HasTimer timer) => Lens' ent timer -> (ent -> ent) -> ent -> m ent
tickOver timerL onTick ent = do
  delta <- deltaTime
  pure $ tickOverBy timerL delta onTick ent

tickOverBy :: (HasTimer timer) => Lens' ent timer -> Seconds -> (ent -> ent) -> ent -> ent
tickOverBy timerL delta onTick =
  execState $ tickBy timerL delta (modify' id) (modify' onTick)

