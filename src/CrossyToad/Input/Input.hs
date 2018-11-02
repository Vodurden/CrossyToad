-- | The @Input@ feature is responsible for gathering input from a human
-- | and making it available to the rest of the application.
-- |
-- | Crucially the @Input@ feature is not responsible for understanding the
-- | intent of the user, it only gathers input such that other features can
-- | interpret it.
module CrossyToad.Input.Input
  ( Input(..)
  , updateInput'
  , getInput'
  , setInput'
  , module CrossyToad.Input.KeyState
  , module CrossyToad.Input.InputState
  ) where

import Control.Lens (Lens', use, assign, set, (^.))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State
import qualified SDL.Extended as SDL

import           CrossyToad.Input.KeyState
import           CrossyToad.Input.InputState
import qualified CrossyToad.Input.InputState as InputState

class Monad m => Input m where
  updateInput :: m ()
  setInput :: InputState -> m ()
  getInput :: m InputState

updateInput' :: (Input m, MonadIO m) => m ()
updateInput' = do
  input <- getInput
  events <- SDL.pollEvents
  setInput (stepInput events input)

getInput' :: (MonadState s m, HasInputState s) => m InputState
getInput' = use InputState.inputState

setInput' :: (MonadState s m, HasInputState s) => InputState -> m ()
setInput' = assign InputState.inputState

stepInput :: [SDL.Event] -> InputState -> InputState
stepInput events input = foldr stepInputByEvent input events

stepInputByEvent :: SDL.Event -> InputState -> InputState
stepInputByEvent event =
    stepKey SDL.KeycodeReturn InputState.enter
    . stepKey SDL.KeycodeEscape InputState.esc
    . stepKey SDL.KeycodeW InputState.w
    . stepKey SDL.KeycodeA InputState.a
    . stepKey SDL.KeycodeS InputState.s
    . stepKey SDL.KeycodeD InputState.d
    . stepQuit InputState.quit
  where
    stepKey :: SDL.Keycode -> Lens' InputState KeyState -> InputState -> InputState
    stepKey keycode keyStateL inState =
        set keyStateL nextKeystate inState
      where
        currentKeystate = inState ^. keyStateL
        nextKeystate = case currentKeystate of
          Pressed | pressed keycode -> Held
          Held | pressed keycode -> Held
          _ | pressed keycode -> Pressed
          _ | released keycode -> Released
          _ -> Released

    pressed keycode = SDL.keyPressed keycode event
    released keycode = SDL.keyReleased keycode event

    stepQuit :: Lens' InputState Bool -> InputState -> InputState
    stepQuit quitL = set quitL (SDL.quitEvent event)
