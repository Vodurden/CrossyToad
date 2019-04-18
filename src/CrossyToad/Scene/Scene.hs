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
import qualified Data.Text as Text

import CrossyToad.Time.Seconds (Seconds)
import qualified CrossyToad.Time.Seconds as Seconds
import CrossyToad.Time.TickSeconds (TickSeconds(..))
-- import Debug.Trace
import           CrossyToad.Logger.LogLevel (LogLevel(..))
import           CrossyToad.Logger.MonadLogger (MonadLogger, logText)

data Scene' m s = Scene'
  { _state :: s

  , _tickInterval :: Seconds
  , _tickAccumulator :: Seconds
  , _tick :: Seconds -> s -> m s

  , _render :: s -> m ()
  }

data Scene m = forall s. Scene (Scene' m s)

makeClassy ''Scene

mk :: s -> (Seconds -> s -> m s) -> (s -> m ()) -> Scene m
mk state' tick' render' =
  Scene $ Scene' { _state = state'
                 , _tickInterval = (1/20)
                 , _tickAccumulator = 0
                 , _tick = tick'
                 , _render = render'
                 }

tick :: (Monad m, MonadLogger m) => TickSeconds -> Scene m -> m (Scene m)
tick (TickSeconds seconds) (Scene sc) = do
    let interval = (_tickInterval sc)
    let nextAccumulator = (_tickAccumulator sc) + seconds
    logText Debug $ (Text.pack "Next Accumulator: ") <> (Text.pack $ show nextAccumulator)
    (nextState, leftoverTicks) <- (Seconds.fixedStepM interval (_tick sc)) nextAccumulator (_state sc)
    pure $ Scene $ sc { _state = nextState
                      , _tickAccumulator = leftoverTicks
                      }

render :: Scene m -> m ()
render (Scene sc) = _render sc (_state sc)
