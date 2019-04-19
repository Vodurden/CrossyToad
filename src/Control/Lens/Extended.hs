{-# LANGUAGE RankNTypes #-}

module Control.Lens.Extended
  ( module Control.Lens
  , modifyingM
  , lensFoldl'
  ) where

import Control.Lens
import Control.Monad.State (MonadState)
import Data.Foldable (foldl')

-- | Like `modifying` but it works over any mtl-style monadic state.
-- |
-- | Shamelessly stolen from https://stackoverflow.com/a/46113439
modifyingM :: MonadState s m => Lens s s a a -> (a -> m a) -> m ()
modifyingM lens' f = do
  old <- use lens'
  new <- f old
  lens' .= new

lensFoldl' :: (b -> a -> b) -> Lens' s b -> Lens' s [a] -> s -> s
lensFoldl' f entL entsL state =
  state & entL .~ foldl' f (state ^. entL) (state ^. entsL)
