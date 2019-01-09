{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Time.Timer
  ( PeriodicTimer(..)
  , HasPeriodicTimer(..)
  , mk
  , stepBy
  ) where

import Control.Lens

import CrossyToad.Effect.Time.Seconds

-- | Models a state change that should occur at a given
-- | interval.
data PeriodicTimer = PeriodicTimer
  { _duration :: !Seconds     -- ^ How long each period is
  , _currentTime :: !Seconds  -- ^ The current time. Counts down to 0
  }

makeClassy ''PeriodicTimer

mk :: Seconds -> PeriodicTimer
mk duration' = PeriodicTimer
  { _duration = duration'
  , _currentTime = duration'
  }

-- | Steps the timer forward by the given amount of seconds.
-- |
-- | If the timer reaches zero (ticks) then the given function
-- | will be applied to the ent and the timer will be reset.
stepBy :: (HasPeriodicTimer ent)
       => Seconds
       -> (ent -> ent)
       -> ent -> ent
stepBy delta onTick ent' =
  let nextEnt = ent' & (currentTime -~ delta)
  in if (nextEnt ^. periodicTimer . currentTime <= 0)
     then onTick nextEnt
     else nextEnt
