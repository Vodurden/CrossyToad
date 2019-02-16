module CrossyToad.Runner (mainLoop) where

import           Control.Monad (unless)
import           Data.Maybe (isNothing)

import           CrossyToad
import           CrossyToad.Input.MonadInput (stepInput)
import qualified CrossyToad.Scene.MonadScene as MonadScene
import           CrossyToad.Time.MonadTask (pumpTasks)
import           CrossyToad.Time.MonadTime (stepTime, deltaTime)

mainLoop :: CrossyToad ()
mainLoop = do
  stepTime
  stepInput

  dt <- deltaTime
  pumpTasks dt
  currentScene' <- MonadScene.tickCurrentScene

  unless (isNothing currentScene') mainLoop

  -- inputEvents' <- getInputEvents
  -- unless (scene == Scene.Quit || QuitEvent `elem` inputEvents') mainLoop
