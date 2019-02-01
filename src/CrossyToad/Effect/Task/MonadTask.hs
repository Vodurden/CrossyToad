module CrossyToad.Effect.Task.MonadTask
  where

import CrossyToad.Effect.Time.Seconds
import CrossyToad.Effect.Task.Task

class Monad m => MonadTask m where
  pumpTasks :: Seconds -> m ()
  forkTask :: Task m () -> m ()