{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Scene.Scene
  ( Scene
  , HasScene(..)
  , load
  , mk
  , mkNoState
  , mkNoTick
  , handleInput
  , tick
  , render
  ) where

import           Control.Lens.Extended

import           CrossyToad.Time.Seconds (Seconds)
import qualified CrossyToad.Time.Seconds as Seconds
import           CrossyToad.Input.Intents (Intents)

data Scene' m s = Scene'
  { _state :: !s

  , _handleInput :: !(Intents -> s -> m s)

  , _tickInterval :: !Seconds
  , _tickAccumulator :: !Seconds
  , _tick :: !(Seconds -> s -> m s)

  , _render :: !(s -> m ())
  }

data Scene m = forall s. Scene (Scene' m s)

makeClassy ''Scene

load :: (Monad m) => m s -> (Intents -> s -> m s) -> (Seconds -> s -> m s) -> (s -> m ()) -> m (Scene m)
load initialize' handleInput' tick' render' = do
  state' <- initialize'
  pure $ mk state' handleInput' tick' render'

mk :: s -> (Intents -> s -> m s) -> (Seconds -> s -> m s) -> (s -> m ()) -> Scene m
mk state' handleInput' tick' render' =
  Scene $ Scene' { _state = state'
                 , _handleInput = handleInput'
                 , _tickInterval = (1/120)
                 , _tickAccumulator = 0
                 , _tick = tick'
                 , _render = render'
                 }

-- | Create a scene that has no state
mkNoState :: (Monad m) => (Intents -> m ()) -> m () -> Scene m
mkNoState handleInput' render' =
  mk () (flip . const $ handleInput') (const $ pure . id) (const render')

mkNoTick :: (Monad m) => s -> (Intents -> s -> m s) -> (s -> m ()) -> Scene m
mkNoTick state' handleInput' render' =
  mk state' handleInput' (const $ pure . id) render'

handleInput :: (Monad m) => Intents -> Scene m -> m (Scene m)
handleInput intents (Scene sc) = do
  nextState <- (_handleInput sc) intents (_state sc)
  pure $ Scene $ sc { _state = nextState }

tick :: (Monad m) => Seconds -> Scene m -> m (Scene m)
tick seconds (Scene sc) = do
  let interval = (_tickInterval sc)
  let nextAccumulator = (_tickAccumulator sc) + seconds
  (nextState, leftoverTicks) <- (Seconds.fixedTickM interval (_tick sc)) nextAccumulator (_state sc)
  pure $ Scene $ sc { _state = nextState
                    , _tickAccumulator = leftoverTicks
                    }

render :: Scene m -> m ()
render (Scene sc) = _render sc (_state sc)
