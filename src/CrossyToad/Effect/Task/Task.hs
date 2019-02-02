-- | A task is an asynchronous effect that
-- | runs independently from the main game loop
module CrossyToad.Effect.Task.Task
  ( Task
  , wait
  ) where

import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors (Await(..), await)

import CrossyToad.Time.Seconds

type Task m = Coroutine (Await Seconds) m

-- | Make the task wait for some time
wait :: (Monad m) => Seconds -> Task m ()
wait s | s <= 0 = pure ()
       | otherwise = do
           dt <- await
           wait $ s - dt
