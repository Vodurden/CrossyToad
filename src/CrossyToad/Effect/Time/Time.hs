{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module CrossyToad.Effect.Time.Time
  ( Time(..)
  , module CrossyToad.Effect.Time.Seconds
  ) where

import Control.Monad.State.Strict (StateT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans, lift)

import CrossyToad.Effect.Time.Seconds

class Monad m => Time m where
  -- | Marks the end of this timestep and begins a new timestep.
  stepTime :: m ()

  -- | Returns how many seconds have elaspsed between the current timestep
  -- | and the previous timestep.
  deltaTime :: m Seconds

  -- | Default instances to easily derive our MonadTrans instances
  default stepTime :: (MonadTrans t, Time m1, m ~ t m1) => m ()
  stepTime = lift stepTime
  default deltaTime :: (MonadTrans t, Time m1, m ~ t m1) => m Seconds
  deltaTime = lift deltaTime

instance Time m => Time (StateT s m)
instance Time m => Time (ReaderT s m)
