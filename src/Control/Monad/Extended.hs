module Control.Monad.Extended
  ( module Control.Monad
  , whenM
  ) where

import Control.Monad

whenM :: Monad m => m Bool -> m () -> m ()
whenM predicate value = do
  predicate' <- predicate
  if predicate' then value else pure ()
