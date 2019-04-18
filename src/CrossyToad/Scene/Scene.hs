{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Scene.Scene
  ( Scene
  , HasScene(..)
  , mk
  , mkNoState
  , handleInput
  , tick
  , render
  ) where

import           Control.Lens.Extended

import           CrossyToad.Time.Seconds (Seconds)
import qualified CrossyToad.Time.Seconds as Seconds
import           CrossyToad.Time.TickSeconds (TickSeconds(..))
import           CrossyToad.Input.InputState (InputState)

data Scene' m s = Scene'
  { _state :: s

  , _handleInput :: InputState -> s -> m s

  , _tickInterval :: Seconds
  , _tickAccumulator :: Seconds
  , _tick :: Seconds -> s -> m s

  , _render :: s -> m ()
  }

data Scene m = forall s. Scene (Scene' m s)

makeClassy ''Scene

mk :: s -> (InputState -> s -> m s) -> (Seconds -> s -> m s) -> (s -> m ()) -> Scene m
mk state' handleInput' tick' render' =
  Scene $ Scene' { _state = state'
                 , _handleInput = handleInput'
                 , _tickInterval = (1/120)
                 , _tickAccumulator = 0
                 , _tick = tick'
                 , _render = render'
                 }

-- | Create a scene that has no state
mkNoState :: (Monad m) => (InputState -> m ()) -> m () -> Scene m
mkNoState handleInput' render' =
  mk () (flip . const $ handleInput') (const $ pure . id) (const render')

handleInput :: (Monad m) => InputState -> Scene m -> m (Scene m)
handleInput inputState (Scene sc) = do
  nextState <- (_handleInput sc) inputState (_state sc)
  pure $ Scene $ sc { _state = nextState }

tick :: (Monad m) => TickSeconds -> Scene m -> m (Scene m)
tick (TickSeconds seconds) (Scene sc) = do
  let interval = (_tickInterval sc)
  let nextAccumulator = (_tickAccumulator sc) + seconds
  (nextState, leftoverTicks) <- (Seconds.fixedStepM interval (_tick sc)) nextAccumulator (_state sc)
  pure $ Scene $ sc { _state = nextState
                    , _tickAccumulator = leftoverTicks
                    }

render :: Scene m -> m ()
render (Scene sc) = _render sc (_state sc)
