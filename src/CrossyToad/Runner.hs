module CrossyToad.Runner (mainLoop) where

import           Control.Monad (unless)

import           CrossyToad
import           CrossyToad.Input.InputEvent (InputEvent(..))
import           CrossyToad.Input.MonadInput (stepInput, getInputEvents)
import qualified CrossyToad.Scene.Scene as Scene
import           CrossyToad.Time.MonadTask (pumpTasks)
import           CrossyToad.Time.MonadTime (stepTime, deltaTime)

mainLoop :: CrossyToad ()
mainLoop = do
  stepTime
  stepInput

  dt <- deltaTime
  pumpTasks dt
  scene <- Scene.run

  inputEvents' <- getInputEvents
  unless (scene == Scene.Quit || QuitEvent `elem` inputEvents') mainLoop
