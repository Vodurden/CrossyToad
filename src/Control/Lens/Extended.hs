{-# LANGUAGE RankNTypes #-}

module Control.Lens.Extended
  ( module Control.Lens
  , modifyingM
  ) where

import Control.Lens
import Control.Monad.State (MonadState)

-- | Like `modifying` but it works over any mtl-style monadic state.
-- |
-- | Shamelessly stolen from https://stackoverflow.com/a/46113439
modifyingM :: MonadState s m => Lens s s a a -> (a -> m a) -> m ()
modifyingM lens' f = do
  old <- use lens'
  new <- f old
  lens' .= new
