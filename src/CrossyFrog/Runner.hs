module CrossyFrog.Runner (mainLoop) where

import Control.Monad (unless)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)

import           CrossyFrog.Config
import           CrossyFrog.State
import           CrossyFrog.Effect.Renderer
import           CrossyFrog.Effect.Input
import qualified CrossyFrog.Engine.InputState as InputState

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
  drawScreen

  unless (InputState.quit input) mainLoop
