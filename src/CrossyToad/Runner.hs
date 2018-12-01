module CrossyToad.Runner (mainLoop) where

import Control.Monad (unless)

import           CrossyToad
import           CrossyToad.Renderer.Renderer
import           CrossyToad.Input.Input
import           CrossyToad.Time.Time
import qualified CrossyToad.Scene.Scene as Scene

mainLoop :: CrossyToad ()
mainLoop = do
  stepTime
  stepInput

  clearScreen

  scene <- Scene.stepIO

  drawScreen

  unless (scene == Scene.Quit) mainLoop
