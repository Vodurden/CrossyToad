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
import Control.Monad.State.Strict.Extended (runStateT)

import CrossyToad.Time.Tick (Tick)
import CrossyToad.Time.MonadTime
import qualified CrossyToad.Time.Tick as Tick

data Scene' m s = Scene'
  { _state :: s
  , _tick :: Tick m s
  , _render :: Tick m s
  }

data Scene m = forall s. Scene (Scene' m s)

makeClassy ''Scene

mk :: (Monad m) => s -> (s -> m s) -> (s -> m ()) -> Scene m
mk state' tick' render' =
  Scene $ Scene' state'
                 (Tick.interval (1/60) tick')
                 (Tick.interval (1/120) (\s -> render' s >> pure s))

tick :: (Monad m, MonadTime m) => Scene m -> m (Scene m)
tick (Scene sc) = do
  dt <- deltaTime
  (state', nextTick) <- runStateT (Tick.step dt (_state sc)) (_tick sc)
  (state'', nextRender) <- runStateT (Tick.step dt state') (_render sc)
  pure $ Scene sc { _state = state'', _tick = nextTick, _render = nextRender }
  -- nextTick <- runStateT (sc ^. tick) Tick.step dt (sc ^. tick)
  -- nextState <- (_tick sc) (_state sc)
  -- pure $ Scene $ sc { _state = nextState }
