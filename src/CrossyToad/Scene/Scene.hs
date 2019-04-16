{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Scene.Scene
  ( Scene
  , HasScene(..)
  , mk
  , tick
  , render
  ) where

import Control.Lens.Extended

import CrossyToad.Time.TickSeconds (TickSeconds)

data Scene' m s = Scene'
  { _state :: s

  , _tick :: TickSeconds -> s -> m s

  , _render :: s -> m ()
  }

data Scene m = forall s. Scene (Scene' m s)

makeClassy ''Scene

mk :: s -> (TickSeconds -> s -> m s) -> (s -> m ()) -> Scene m
mk state' tick' render' = Scene $ Scene' state' tick' render'

tick :: (Monad m) => TickSeconds -> Scene m -> m (Scene m)
tick seconds (Scene sc) = do
  nextState <- _tick sc seconds (_state sc)
  pure $ Scene $ sc { _state = nextState }

render :: Scene m -> m ()
render (Scene sc) = _render sc (_state sc)
