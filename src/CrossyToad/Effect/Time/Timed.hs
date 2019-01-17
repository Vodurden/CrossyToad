{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module CrossyToad.Effect.Time.Timed where

import Control.Lens
import Control.Monad.State.Extended (StateT, State, execState, execStateT, hoistState)
import qualified Data.List as List

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
-- state %= (Timed.now $ const CoolingDown) >>> (Timed.after 5 $ const Ready)
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
  {
  -- | The amount of time until the next event
    _nextEventTime :: !Seconds

  -- | The changes to apply at a given time.
  -- |
  -- | _1: The time to apply the change, relative to the previous event
  -- | _2: The value to change to
  -- |
  -- | We assume this list is sorted by _1
  , _events :: [Event a]

  -- | The value _right now_
  , _value :: !a
  } deriving (Eq, Show)

data Event a = Event
  { _eventTime :: !Seconds
  , _eventValue :: !a
  } deriving (Eq, Show)

makeClassy ''Timed
makeClassy ''Event

instance Functor Timed where
  fmap f timed' = Timed
    { _nextEventTime = timed' ^. nextEventTime
    , _events = (timed' ^. events) & each %~ fmap f
    , _value = f $ timed' ^. value
    }

instance Functor Event where
  fmap f event' = Event (event' ^. eventTime) (f $ event' ^. eventValue)

instance (Eq a) => Ord (Event a) where
  compare e1 e2 = compare (e1 ^. eventTime) (e2 ^. eventTime)

mk :: a -> Timed a
mk a = Timed
  { _nextEventTime = 0
  , _events = []
  , _value = a
  }

-- | Immediately transition to a new value
immediate :: a -> Timed a -> Timed a
immediate value' = value .~ value'

-- | Schedule an event at the current time
now :: (Eq a) => a -> Timed a -> Timed a
now value' = after 0 value'

-- | Schedule an event at the given relative time
-- |
-- | Note: Relative time is relative to the current time when
-- | this function is called as denoted by the `_currentTime` field
after :: (Eq a) => Seconds -> a -> Timed a -> Timed a
after time value' t = t & (events %~ List.insert (Event time value'))

-- | Repeat the current events infinitely
loop :: Timed a -> Timed a
loop timed' = timed' & events %~ List.cycle

step :: (Time m, HasTimed ent a) => StateT ent m a
step = do
  delta <- deltaTime
  hoistState $ stepBy delta

step_ :: (Time m, HasTimed ent a) => ent -> m ent
step_ = execStateT step

-- | Advances the timer by the amount of time given.
stepBy :: (HasTimed ent a) => Seconds -> State ent a
stepBy delta = do
  timed' <- use timed
  case timed' ^. events of
    [] -> pure (timed' ^. value)
    (nextEvent : rest) -> do
      nextEventTime += delta
      nextEventTime' <- use nextEventTime
      if nextEventTime' < (nextEvent ^. eventTime)
      then pure (timed' ^. value)
      else do
        nextEventTime .= 0
        events .= rest
        value .= (nextEvent ^. eventValue)
        use value

stepBy_ :: (HasTimed ent a) => Seconds -> ent -> ent
stepBy_ = execState . stepBy
