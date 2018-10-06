module CrossyToad.Runner (mainLoop) where

import Control.Monad (unless)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)

import           CrossyToad.Config
import           CrossyToad.State
import           CrossyToad.Effect.Renderer
import           CrossyToad.Effect.Input
import qualified CrossyToad.Engine.InputState as InputState

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
