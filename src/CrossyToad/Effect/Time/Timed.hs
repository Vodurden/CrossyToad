{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module CrossyToad.Effect.Time.Timed where

import Control.Lens
import Control.Monad.State.Extended (StateT, State, execState, execStateT, hoistState)

import CrossyToad.Effect.Time.Time

-- Consider soemething like:
--
-- data Timed a = Timed
--   { _currentTime :: Seconds
--   , _lastEventTime :: Seconds
--   , _events :: [(Seconds, a -> a)]
--   , _value :: a
--   }
--
-- I.e. the state change is just a set of events
-- denoting a change that occurs after a particular
-- time
--
-- state %= (Timed.now $ const CoolingDown) . (Timed.after 5 $ const Ready)
--
-- frame %= Timed.after (frame^.focus.seconds) nextFrame
--
-- spawnTimer %= Timed.loop . (Timed.next []) . (Timed.spread [0,1,2] [Car])
--
-- Q: How do we deal with "after"? Should all "now"/"next" be relative to the last event?
-- Q: How do we deal with overlap? If we make everything relative that will solve the issue,
--    or we can compose all the overlapping functions
-- Q: How do we ensure we don't miss events? Or do we even care?
--    Maybe force only transitioning by a single event per step?

-- | Represents a value that changes over time.
data Timed a = Timed
  { _whenRunning :: !a
  , _onStopped :: !a
  , _whenStopped :: !a

  , _startTime :: !Seconds
  , _state :: !TimedState
  } deriving (Eq, Show)

data TimedState
  = Running !Seconds
  | JustStopped
  | Stopped
  deriving (Eq, Show)

makeClassy ''Timed
makeClassyPrisms ''TimedState

mk :: Seconds -> a -> Timed a
mk startTime' a = Timed
  { _whenRunning = a
  , _onStopped = a
  , _whenStopped = a

  , _startTime = startTime'
  , _state = Stopped
  }

-- | Starts the timer. If the timer has already started this will restart it.
start :: (HasTimed ent a) => ent -> ent
start ent' = ent' & timed . state .~ (Running startTime')
  where
    startTime' = ent' ^. timed . startTime

-- | Stops the timer.
stop :: (HasTimed ent a) => ent -> ent
stop ent' = ent' & timed . state .~ JustStopped

-- | Returns the value at the current time
value :: (HasTimed ent a) => ent -> a
value ent' =
  case ent' ^. timed . state of
    (Running _) -> (ent' ^. timed . whenRunning)
    JustStopped -> (ent' ^. timed . onStopped)
    Stopped -> (ent' ^. timed . whenStopped)

tick :: (Time m, HasTimed ent a) => StateT ent m a
tick = do
  delta <- deltaTime
  hoistState $ tickBy delta

tick_ :: (Time m, HasTimed ent a) => ent -> m ent
tick_ = execStateT tick

-- | Advances the timer by the amount of time given.
tickBy :: (HasTimed ent a) => Seconds -> State ent a
tickBy delta = do
  timed . state %= \case
    (Running currentTime) ->
      let nextTime = max 0 (currentTime - delta)
      in if (nextTime > 0)
         then (Running nextTime)
         else JustStopped
    JustStopped -> Stopped
    Stopped -> Stopped
  uses timed value

tickBy_ :: (HasTimed ent a) => Seconds -> ent -> ent
tickBy_ = execState . tickBy
