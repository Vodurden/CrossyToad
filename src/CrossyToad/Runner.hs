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

-- -- | Extra effects to run when transitioning from one scene to another
-- transition :: (HasScene ent, HasGameState ent) => Scene -> Scene -> ent -> ent
-- transition oldScene newScene | oldScene /= newScene = Scene.initialize
--                              | otherwise = id
