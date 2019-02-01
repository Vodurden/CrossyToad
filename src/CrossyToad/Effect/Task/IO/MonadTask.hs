module CrossyToad.Effect.Task.IO.MonadTask
  ( forkTask
  , pumpTasks
  ) where

import Control.Lens
import Control.Monad.Coroutine (resume)
import Control.Monad.Coroutine.SuspensionFunctors (Await(..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (catMaybes)
import Data.IORef (modifyIORef', readIORef, writeIORef)

import CrossyToad.Effect.Task.IO.Env
import CrossyToad.Effect.Task.IO.TaskState
import CrossyToad.Effect.Task.Task
import CrossyToad.Effect.Time.Seconds

-- | Start a new task
forkTask :: (MonadReader r m, HasEnv r m, MonadIO m) => Task m () -> m ()
forkTask task' = do
  taskStateRef' <- view (env.taskStateRef)
  liftIO $ modifyIORef' taskStateRef' (tasks %~ (task' :))

-- | Run all the currently active tasks for the amount of time given
pumpTasks :: (MonadReader r m, HasEnv r m, MonadIO m) => Seconds -> m ()
pumpTasks dt = do
  taskStateRef' <- view (env.taskStateRef)
  taskState' <- liftIO $ readIORef taskStateRef'
  let tasks' = taskState' ^. tasks
  newTasks <- (flip traverse) tasks' $ \task -> do
    z <- resume task
    pure $ case z of
      Left (Await f) -> Just $ f dt
      Right _ -> Nothing

  let newState = taskState' & tasks .~ (catMaybes newTasks)
  liftIO $ writeIORef taskStateRef' newState
