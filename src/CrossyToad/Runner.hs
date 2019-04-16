module CrossyToad.Runner (mainLoop) where

import           Control.Monad (unless)
import           Data.Maybe (isNothing)

import           CrossyToad
import           CrossyToad.Input.MonadInput (stepInput)
import qualified CrossyToad.Scene.MonadScene as MonadScene
import           CrossyToad.Time.MonadTask (pumpTasks)
import           CrossyToad.Time.MonadTime (stepTime, deltaTime)
import           CrossyToad.Time.TickSeconds (TickSeconds(..))

mainLoop :: CrossyToad ()
mainLoop = do
  stepTime
  stepInput

  dt <- deltaTime
  pumpTasks dt

  currentScene' <- MonadScene.tickCurrentScene (TickSeconds dt)

  unless (isNothing currentScene') mainLoop
