{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Scene.Scene
  ( Scene
  , HasScene(..)
  , mk
  , tick
  ) where

import           Control.Lens.Extended

data Scene' m s = Scene'
  { _state :: s
  , _tick :: s -> m s
  }

data Scene m = forall s. Scene (Scene' m s)

makeClassy ''Scene

mk :: s -> (s -> m s) -> Scene m
mk state' tick' = Scene $ Scene' state' tick'

tick :: (Monad m) => Scene m -> m (Scene m)
tick (Scene sc) = do
  nextState <- (_tick sc) (_state sc)
  pure $ Scene $ sc { _state = nextState }
