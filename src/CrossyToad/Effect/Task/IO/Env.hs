{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Effect.Task.IO.Env where

import           Control.Lens
import           Data.IORef

import           CrossyToad.Effect.Task.IO.TaskState (TaskState, HasTaskStateIORef(..))
import qualified CrossyToad.Effect.Task.IO.TaskState as TaskState

data Env m = Env
  { __taskStateRef :: IORef (TaskState m)
  }

makeClassy ''Env

instance HasTaskStateIORef m (Env m) where taskStateRef = _taskStateRef

initialize :: IO (Env m)
initialize = Env <$> (newIORef TaskState.mk)
