{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Time.MonadTask.IO.Env where

import           Control.Lens
import           Data.IORef

import           CrossyToad.Time.MonadTask.IO.TaskState (TaskState, HasTaskStateIORef(..))
import qualified CrossyToad.Time.MonadTask.IO.TaskState as TaskState

data Env m = Env
  { __taskStateRef :: IORef (TaskState m)
  }

makeClassy ''Env

instance HasTaskStateIORef m (Env m) where taskStateRef = _taskStateRef

initialize :: IO (Env m)
initialize = Env <$> (newIORef TaskState.mk)
