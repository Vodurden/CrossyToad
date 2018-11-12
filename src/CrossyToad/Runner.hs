module CrossyToad.Runner (mainLoop) where

import Control.Lens (use)
import Control.Monad (unless)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)

import           CrossyToad.Config
import           CrossyToad.Vars (HasVars)
import           CrossyToad.Renderer.Renderer
import           CrossyToad.Input.Input
import           CrossyToad.Time.Time
import           CrossyToad.Scene.Scene (Scene, HasScene)
import qualified CrossyToad.Scene.Scene as Scene
import           CrossyToad.Scene.Game.GameState (HasGameState)

mainLoop ::
  ( MonadReader Config m, MonadState s m
  , HasVars s
  , HasScene s
  , HasGameState s
  , Input m
  , Renderer m
  , Time m
  ) => m ()
mainLoop = do
  stepTime
  stepInput

  oldScene <- use Scene.scene
  clearScreen
  Scene.step
  drawScreen

  scene <- use Scene.scene
  transition oldScene scene
  unless (scene == Scene.Quit) mainLoop

-- | Extra effects to run when transitioning from one scene to another
transition :: (MonadState s m, HasScene s, HasGameState s) => Scene -> Scene -> m ()
transition oldScene newScene | oldScene /= newScene = Scene.initialize
                             | otherwise = pure ()
