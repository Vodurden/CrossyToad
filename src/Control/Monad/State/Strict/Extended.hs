module Control.Monad.State.Strict.Extended
  ( module Control.Monad.State.Strict
  , hoistState
  ) where

import Control.Monad.State.Strict

hoistState :: Monad m => State s a -> StateT s m a
hoistState = state . runState
