module CrossyToad.Runner (mainLoop) where

import Control.Lens
import Control.Monad (unless)
import Control.Monad.State.Extended (State, execStateT, execState, get)

import           CrossyToad
import           CrossyToad.Vars (HasVars(..))
import           CrossyToad.Renderer.Renderer
import           CrossyToad.Input.Input
import           CrossyToad.Time.Time
import           CrossyToad.Scene.Scene (Scene, HasScene)
import qualified CrossyToad.Scene.Scene as Scene
import           CrossyToad.Scene.Game.GameState (HasGameState)

mainLoop :: CrossyToad ()
mainLoop = do
  stepTime
  stepInput

  clearScreen

  oldScene <- use Scene.scene

  vars' <- get
  newVars <- execStateT Scene.step vars'
  vars .= newVars

  scene <- use Scene.scene
  let newVars' = execState (transition oldScene scene) newVars
  vars .= newVars'

  drawScreen

  unless (scene == Scene.Quit) mainLoop

-- | Extra effects to run when transitioning from one scene to another
transition :: (HasScene s, HasGameState s) => Scene -> Scene -> State s ()
transition oldScene newScene | oldScene /= newScene = Scene.initialize
                             | otherwise = pure ()
