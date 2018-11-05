module CrossyToad.Time.Time
  ( Time(..)
  , module CrossyToad.Time.Seconds
  ) where

import CrossyToad.Time.Seconds

class Monad m => Time m where
  -- | Marks the end of this timestep and begins a new timestep.
  stepTime :: m ()

  -- | Returns how many seconds have elaspsed between the current timestep
  -- | and the previous timestep.
  deltaTime :: m Seconds
