module CrossyToad.Scene.Game.Game
  ( initialize
  , stepGame
  , stepIntent
  , module CrossyToad.Scene.Game.GameState
  ) where

import           Control.Lens.Extended
import           Control.Monad (forM_, (>=>))
import           Data.Foldable (foldl')
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
import           CrossyToad.Scene.Game.Car (HasCars(..))
import qualified CrossyToad.Scene.Game.Car as Car
import qualified CrossyToad.Scene.Game.Collision as Collision
import qualified CrossyToad.Scene.Game.SpawnPoint as SpawnPoint
import           CrossyToad.Scene.Game.SpawnPoint (HasSpawnPoints(..))

initialize :: (HasGameState ent) => ent -> ent
initialize =
  (gameState.toad .~ Toad.mk (V2 (7*64) (13*64)))
  . (gameState.cars .~ [])
  . (gameState.spawnPoints .~
      [ SpawnPoint.mk (V2 0 1*64) East 1 1
      , SpawnPoint.mk (V2 (14*64) (3*64)) West 1 1
      ])

stepGame :: (Input m, Renderer m, Time m, HasGameState ent, HasScene ent) => ent -> m ent
stepGame ent = do
  ent' <- stepIntents ent
  ent'' <- stepGameState ent'
  renderGame ent''
  pure ent''

-- | Update the GameState and Scene based on the intent of the user
stepIntents :: (Input m, HasGameState ent, HasScene ent) => ent -> m ent
stepIntents ent = do
  inputState' <- getInputState
  pure $ foldl' (flip stepIntent) ent (Intent.fromInputState inputState')

stepIntent :: (HasGameState ent, HasScene ent) => Intent -> ent -> ent
stepIntent (Move dir) = gameState.toad %~ (Toad.jump dir)
stepIntent Exit = scene .~ Scene.Title

-- | Step all the GameState specific logic
stepGameState :: (Input m, Time m, HasGameState ent) => ent -> m ent
stepGameState =
  mapMOf gameState $
    Toad.step
    >=> SpawnPoint.stepAll
    >=> Car.stepAll
    >=> (pure . Collision.step)

-- | Draws the Scene on the screen
renderGame :: (HasGameState ent, Renderer m) => ent -> m ()
renderGame ent = do
  forM_ (ent ^. gameState . cars) Car.render
  Toad.render (ent ^. gameState . toad)
