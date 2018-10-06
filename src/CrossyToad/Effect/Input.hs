module CrossyToad.Effect.Input where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State
import qualified SDL

import CrossyToad.State
import CrossyToad.Engine.InputState

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
getInput' = gets vInput

setInput' :: MonadState Vars m => InputState -> m ()
setInput' input = modify (\v -> v { vInput = input })

stepInput :: [SDL.Event] -> InputState -> InputState
stepInput events originalInput = foldr stepByEvent originalInput (SDL.eventPayload <$> events)
  where stepByEvent :: SDL.EventPayload -> InputState -> InputState
        stepByEvent (SDL.KeyboardEvent event) input =
          if SDL.keyboardEventKeyMotion event == SDL.Pressed &&
             SDL.keysymKeycode (SDL.keyboardEventKeysym event) == SDL.KeycodeQ
          then input { quit = True }
          else input
        stepByEvent _ input = input
