module CrossyToad.Runner (mainLoop) where

import Control.Monad (unless)

import           CrossyToad
import           CrossyToad.Effect.Input.Input (InputEvent(..), stepInput, getInputEvents)
import           CrossyToad.Time.MonadTask (pumpTasks)
import           CrossyToad.Time.MonadTime (stepTime, deltaTime)
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
