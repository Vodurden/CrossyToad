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
  -- | Marks the end of this tick and begins a new tick.
  tickTime :: m ()

  -- | Returns how many seconds have elaspsed between the current timetick
  -- | and the previous timetick.
  deltaTime :: m Seconds

  -- | Default instances to easily derive our MonadTrans instances
  default tickTime :: (MonadTrans t, MonadTime m1, m ~ t m1) => m ()
  tickTime = lift tickTime
  default deltaTime :: (MonadTrans t, MonadTime m1, m ~ t m1) => m Seconds
  deltaTime = lift deltaTime

instance MonadTime m => MonadTime (StateT s m)
instance MonadTime m => MonadTime (ReaderT s m)
