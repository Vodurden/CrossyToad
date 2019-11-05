{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

module CrossyToad.Time.Timed where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.State.Strict.Extended (State, execState)
import qualified Data.List as List

import           CrossyToad.Time.Seconds

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
  , _events :: ![Event a]

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
now :: a -> Timed a -> Timed a
now value' = after 0 value'

-- | Schedule the timer to have a value for one frame
pulse :: Seconds -> a -> Timed (Maybe a) -> Timed (Maybe a)
pulse time value' = (after time $ Just value') >>> (now Nothing)

-- | Schedule an event at the given relative time
-- |
-- | Note: Relative time is relative to the current time when
-- | this function is called as denoted by the `_currentTime` field
after :: Seconds -> a -> Timed a -> Timed a
after time value' t = t & (events %~ (++ [Event time value']))

-- | Repeat the current events infinitely
loop :: Timed a -> Timed a
loop timed' = timed' & events %~ List.cycle

-- | Advances the timer by the amount of time given.
tick :: (HasTimed ent a) => Seconds -> State ent a
tick delta = do
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

tick_ :: (HasTimed ent a) => Seconds -> ent -> ent
tick_ = execState . tick
