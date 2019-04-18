module CrossyToad.Runner (mainLoop) where

import           Control.Monad (unless)
import           Data.Maybe (isNothing)

import           CrossyToad
import           CrossyToad.Input.MonadInput (stepInput)
import qualified CrossyToad.Scene.MonadScene as MonadScene
import           CrossyToad.Logger.LogLevel (LogLevel(..))
import           CrossyToad.Logger.MonadLogger (logText)
import           CrossyToad.Time.MonadTask (pumpTasks)
import           CrossyToad.Time.MonadTime (stepTime, deltaTime)
import           CrossyToad.Time.TickSeconds (TickSeconds(..))

mainLoop :: CrossyToad ()
mainLoop = do
  stepTime
  stepInput

  dt <- deltaTime
  pumpTasks dt

  _ <- MonadScene.handleInputCurrentScene

  logText Debug "Tick Current Scene"
  currentScene' <- MonadScene.tickCurrentScene (TickSeconds dt)

  logText Debug "Render Current Scene"
  MonadScene.renderCurrentScene

  unless (isNothing currentScene') mainLoop
