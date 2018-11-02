module CrossyToad.Scene.Game.Game
  ( stepGame
  , stepIntent
  , module CrossyToad.Scene.Game.GameState
  ) where

import           Control.Lens
import           Control.Monad.State (MonadState)
import           Data.Foldable (traverse_)

import           CrossyToad.Input.Input
import           CrossyToad.Renderer.Renderer
import           CrossyToad.Scene.Internal (HasScene, scene)
import qualified CrossyToad.Scene.Internal as Scene
import           CrossyToad.Scene.Game.Intent (Intent(..))
import qualified CrossyToad.Scene.Game.Intent as Intent
import           CrossyToad.Scene.Game.GameState
import           CrossyToad.Scene.Game.Toad
import qualified CrossyToad.Scene.Game.Toad as Toad
import           CrossyToad.Scene.Game.Position

stepGame :: (MonadState s m, HasGameState s, HasScene s, Input m, Renderer m) => m ()
stepGame = do
  input <- getInput
  traverse_ stepIntent (Intent.fromInput input)

  gameState.toad %= Toad.step

  renderGame

stepIntent :: (MonadState s m, HasScene s, HasGameState s) => Intent -> m ()
stepIntent (Move direction) = gameState.toad %= Toad.jump direction
stepIntent Exit = scene .= Scene.Title

renderGame :: (MonadState s m, HasGameState s, Renderer m) => m ()
renderGame = do
  pos <- use $ gameState.toad.position
  drawToad pos
