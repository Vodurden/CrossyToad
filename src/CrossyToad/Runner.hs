module CrossyToad.Runner (mainLoop) where

import           Control.Monad (unless)
import           Data.Maybe (isNothing)

import           CrossyToad
import           CrossyToad.Input.MonadInput (tickInput)
import qualified CrossyToad.Scene.MonadScene as MonadScene
import           CrossyToad.Time.MonadTask (pumpTasks)
import           CrossyToad.Time.MonadTime (tickTime, deltaTime)

mainLoop :: CrossyToad ()
mainLoop = do
  tickTime
  dt <- deltaTime
  pumpTasks dt

  intent <- tickInput
  _ <- MonadScene.handleInputCurrentScene intent

  currentScene' <- MonadScene.tickCurrentScene dt

  MonadScene.renderCurrentScene

  unless (isNothing currentScene') mainLoop
