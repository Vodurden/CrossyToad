module CrossyToad.Effect.Input where

import Control.Lens (Lens', use, assign, set, (^.))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State
import qualified SDL.Extended as SDL

import           CrossyToad.Vars (Vars)
import qualified CrossyToad.Vars as Vars
import           CrossyToad.Engine.KeyState
import           CrossyToad.Engine.InputState (InputState)
import qualified CrossyToad.Engine.InputState as InputState

class Monad m => Input m where
  updateInput :: m ()
  setInput :: InputState -> m ()
  getInput :: m InputState

updateInput' :: (Input m, MonadIO m) => m ()
updateInput' = do
  input <- getInput
  events <- SDL.pollEvents
  setInput (stepInput events input)

getInput' :: MonadState Vars m => m InputState
getInput' = use Vars.input

setInput' :: MonadState Vars m => InputState -> m ()
setInput' input = assign Vars.input input

stepInput :: [SDL.Event] -> InputState -> InputState
stepInput events input = foldr stepInputByEvent input events

stepInputByEvent :: SDL.Event -> InputState -> InputState
stepInputByEvent event =
    stepKey SDL.KeycodeReturn InputState.enter
    . stepQuit InputState.quit
  where
    stepKey :: SDL.Keycode -> Lens' InputState KeyState -> InputState -> InputState
    stepKey keycode keyStateL inputState =
        set keyStateL nextKeystate inputState
      where
        currentKeystate = inputState ^. keyStateL
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
