module Control.Monad.State.Extended
  ( module Control.Monad.State
  , hoistState
  ) where

import Control.Monad.State

hoistState :: Monad m => State s a -> StateT s m a
hoistState = state . runState
