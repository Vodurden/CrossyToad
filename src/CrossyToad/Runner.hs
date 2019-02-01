module CrossyToad.Runner (mainLoop) where

import Control.Monad (unless)

import           CrossyToad
import           CrossyToad.Effect.Input.Input (InputEvent(..), stepInput, getInputEvents)
import           CrossyToad.Effect.Task.MonadTask (pumpTasks)
import           CrossyToad.Effect.Time.Time (stepTime, deltaTime)
import qualified CrossyToad.Scene.Scene as Scene

mainLoop :: CrossyToad ()
mainLoop = do
  stepTime
  stepInput

  dt <- deltaTime
  pumpTasks dt
  scene <- Scene.run

  inputEvents' <- getInputEvents
  unless (scene == Scene.Quit || QuitEvent `elem` inputEvents') mainLoop
