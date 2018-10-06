module CrossyToad.Runner (mainLoop) where

import Control.Monad (unless)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState, gets)

import           CrossyToad.Config
import           CrossyToad.State
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

  scene <- gets vScene
  Scene.step scene

  drawScreen

  unless (InputState.quit input) mainLoop
