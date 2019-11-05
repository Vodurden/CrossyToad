{-# LANGUAGE RankNTypes #-}

module Control.Lens.Extended
  ( module Control.Lens
  , modifyingM
  , lensFoldl'
  , lensMapAccumL
  ) where

import Control.Lens
import Control.Monad.State.Strict (MonadState)
import Data.Foldable (foldl')
import Data.Traversable (mapAccumL)

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

lensMapAccumL :: (b -> a -> (b, a)) -> Lens' s b -> Lens' s [a] -> s -> s
lensMapAccumL f entL entsL state =
  let (newEnt, newEnts) = mapAccumL f (state ^. entL) (state ^. entsL)
  in state & (entL .~ newEnt) . (entsL .~ newEnts)
