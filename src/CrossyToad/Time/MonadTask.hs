module CrossyToad.Time.MonadTask
  ( MonadTask(..)
  ) where

import CrossyToad.Time.Task
import CrossyToad.Time.Seconds

class Monad m => MonadTask m where
  pumpTasks :: Seconds -> m ()
  forkTask :: Task m () -> m ()
