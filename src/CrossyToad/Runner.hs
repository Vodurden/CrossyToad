module CrossyToad.Runner (mainLoop) where

import Control.Monad (unless)

import           CrossyToad
import           CrossyToad.Effect.Input.Input (InputEvent(..), stepInput, getInputEvents)
import           CrossyToad.Effect.Time.Time (stepTime)
import qualified CrossyToad.Scene.Scene as Scene

mainLoop :: CrossyToad ()
mainLoop = do
  stepTime
  stepInput

  scene <- Scene.run

  inputEvents' <- getInputEvents
  unless (scene == Scene.Quit || QuitEvent `elem` inputEvents') mainLoop
