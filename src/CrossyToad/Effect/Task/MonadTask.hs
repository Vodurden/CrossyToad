module CrossyToad.Effect.Task.MonadTask
  where

import CrossyToad.Effect.Task.Task
import CrossyToad.Time.Seconds

class Monad m => MonadTask m where
  pumpTasks :: Seconds -> m ()
  forkTask :: Task m () -> m ()
