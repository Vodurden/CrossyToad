module CrossyToad.Scene.Game.Game
  ( initialize
  , stepGame
  , stepIntent
  , module CrossyToad.Scene.Game.GameState
  ) where

import           Control.Lens
import           Control.Monad.State (MonadState)
import           Data.Foldable (traverse_)
import           Linear.V2

import           CrossyToad.Physics.Physics (Direction(..))
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

initialize :: (MonadState s m, HasGameState s) => m ()
initialize = do
  gameState.toad .= Toad.mk (V2 (7*64) (13*64))
  gameState.cars.=
    [ Car.mk (V2 0 1*64) East
    , Car.mk (V2 0 2*64) East
    , Car.mk (V2 (14*64) (3*64)) West
    ]

-- | Steps the state of the game.
-- |
-- | Should be executed once per frame when this scene is active.
stepGame :: (MonadState s m, HasGameState s, HasScene s, Input m, Renderer m, Time m) => m ()
stepGame = do
  inputState' <- getInputState
  traverse_ stepIntent (Intent.fromInputState inputState')

  gameState' <- use gameState

  nextToad <- Toad.step (gameState' ^. toad)
  gameState.toad .= nextToad

  nextCars <- traverse Car.step (gameState' ^. cars)
  gameState.cars .= nextCars

  stepCollisions

  renderGame

-- | Applies the intent of the user to the scene
stepIntent :: (MonadState s m, HasScene s, HasGameState s) => Intent -> m ()
stepIntent (Move dir) = gameState.toad %= Toad.jump dir
stepIntent Exit = scene .= Scene.Title

stepCollisions :: (MonadState s m, HasGameState s) => m ()
stepCollisions = do
  gameState' <- use gameState

  gameState.toad .= foldl Toad.collision (gameState' ^. toad) (gameState' ^. cars)

-- | Draws the Scene on the screen
renderGame :: (MonadState s m, HasGameState s, Renderer m) => m ()
renderGame = do
  gameState' <- use gameState

  traverse_ Car.render (gameState' ^. cars)

  Toad.render (gameState' ^. toad)
