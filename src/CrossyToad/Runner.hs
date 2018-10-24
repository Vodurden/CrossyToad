module CrossyToad.Runner (mainLoop) where

import Control.Lens ((^.), use)
import Control.Monad (unless)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)

import           CrossyToad.Config
import           CrossyToad.State (Vars)
import qualified CrossyToad.State as Vars
import           CrossyToad.Effect.Renderer
import           CrossyToad.Effect.Input
import qualified CrossyToad.Engine.InputState as InputState
import qualified CrossyToad.Scene.Scene as Scene

mainLoop ::
  ( MonadReader Config m
  , MonadState Vars m
  , Input m
  , Renderer m
  ) => m ()
mainLoop = do
  updateInput
  input <- getInput

  clearScreen

  scene <- use Vars.scene
  Scene.step scene

  drawScreen

  unless (input ^. InputState.quit) mainLoop
