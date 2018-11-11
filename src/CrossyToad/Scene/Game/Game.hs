module CrossyToad.Scene.Game.Game
  ( stepGame
  , stepIntent
  , module CrossyToad.Scene.Game.GameState
  ) where

import           Control.Lens
import           Control.Monad.State (MonadState)
import           Data.Foldable (traverse_)

import           CrossyToad.Input.Input (Input(..))
import           CrossyToad.Renderer.Renderer (Renderer(..))
import           CrossyToad.Time.Time (Time(..))
import           CrossyToad.Scene.Internal (HasScene, scene)
import qualified CrossyToad.Scene.Internal as Scene
import           CrossyToad.Scene.Game.Intent (Intent(..))
import qualified CrossyToad.Scene.Game.Intent as Intent
import           CrossyToad.Scene.Game.GameState
import           CrossyToad.Scene.Game.Toad
import qualified CrossyToad.Scene.Game.Toad as Toad
import qualified CrossyToad.Scene.Game.Car as Car

stepGame :: (MonadState s m, HasGameState s, HasScene s, Input m, Renderer m, Time m) => m ()
stepGame = do
  inputState' <- getInputState
  traverse_ stepIntent (Intent.fromInputState inputState')

  gameState' <- use gameState

  nextToad <- Toad.step (gameState' ^. toad)
  gameState.toad .= nextToad

  nextCars <- traverse Car.step (gameState' ^. cars)
  gameState.cars .= nextCars

  renderGame

stepIntent :: (MonadState s m, HasScene s, HasGameState s) => Intent -> m ()
stepIntent (Move dir) = gameState.toad %= Toad.jump dir
stepIntent Exit = scene .= Scene.Title

renderGame :: (MonadState s m, HasGameState s, Renderer m) => m ()
renderGame = do
  gameState' <- use gameState

  Toad.render (gameState' ^. toad)

  traverse_ Car.render (gameState' ^. cars)
