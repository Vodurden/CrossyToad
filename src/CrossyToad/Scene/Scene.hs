{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Scene.Scene
  ( Scene
  , HasScene(..)
  , mk
  , tick
  ) where

import Control.Lens.Extended

import CrossyToad.Time.TickSeconds (TickSeconds)

data Scene' m s = Scene'
  { _state :: s
  , _tick :: TickSeconds -> s -> m s
  }

data Scene m = forall s. Scene (Scene' m s)

makeClassy ''Scene

mk :: s -> (TickSeconds -> s -> m s) -> Scene m
mk state' tick' = Scene $ Scene' state' tick'

tick :: (Monad m) => TickSeconds -> Scene m -> m (Scene m)
tick seconds (Scene sc) = do
  nextState <- _tick sc seconds (_state sc)
  pure $ Scene $ sc { _state = nextState }
