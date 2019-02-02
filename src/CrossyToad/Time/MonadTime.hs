{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module CrossyToad.Time.MonadTime
  ( MonadTime(..)
  ) where

import Control.Monad.State.Strict (StateT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans, lift)

import CrossyToad.Time.Seconds

class Monad m => MonadTime m where
  -- | Marks the end of this timestep and begins a new timestep.
  stepTime :: m ()

  -- | Returns how many seconds have elaspsed between the current timestep
  -- | and the previous timestep.
  deltaTime :: m Seconds

  -- | Default instances to easily derive our MonadTrans instances
  default stepTime :: (MonadTrans t, MonadTime m1, m ~ t m1) => m ()
  stepTime = lift stepTime
  default deltaTime :: (MonadTrans t, MonadTime m1, m ~ t m1) => m Seconds
  deltaTime = lift deltaTime

instance MonadTime m => MonadTime (StateT s m)
instance MonadTime m => MonadTime (ReaderT s m)
