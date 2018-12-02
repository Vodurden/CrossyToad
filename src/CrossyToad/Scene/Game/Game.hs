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
import           CrossyToad.Effect.Input.Input (Input(..))
import           CrossyToad.Effect.Logger.Logger (Logger(..))
import           CrossyToad.Effect.Renderer.Renderer (Renderer(..))
import qualified CrossyToad.Effect.Renderer.Renderer as Renderer
import qualified CrossyToad.Effect.Renderer.ImageAsset as ImageAsset
import           CrossyToad.Effect.Time.Time (Time(..))
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
import           CrossyToad.Scene.Game.SpawnPoint (SpawnPoint, HasSpawnPoints(..))
import qualified CrossyToad.Scene.Game.SpawnPoint as SpawnPoint

initialize :: (HasGameState ent) => ent -> ent
initialize =
    (gameState.toad .~ Toad.mk (V2 (7*64) (13*64)))
    . (gameState.cars .~ [])
    . (gameState.spawnPoints .~ spawnPoints')
  where
    spawnPoints' :: [SpawnPoint]
    spawnPoints' =
      [ -- River Spawns
        -- TODO

        -- Road Spawns
        SpawnPoint.mk (V2 (20*64) (7*64 )) West 5 1
      , SpawnPoint.mk (V2 (20*64) (8*64 )) West (1.5) 1
      , SpawnPoint.mk (V2 0       (9*64 )) East 3 1
      , SpawnPoint.mk (V2 (20*64) (10*64)) West 2 1
      ]

stepGame :: (Input m, Logger m, Renderer m, Time m, HasGameState ent, HasScene ent) => ent -> m ent
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
stepGameState :: (Input m, Logger m, Time m, HasGameState ent) => ent -> m ent
stepGameState =
  mapMOf gameState $
    Toad.step
    >=> SpawnPoint.stepAll
    >=> Car.stepAll
    >=> Collision.step

-- | Draws the Scene on the screen
renderGame :: (HasGameState ent, Renderer m) => ent -> m ()
renderGame ent = do
  renderBackground
  forM_ (ent ^. gameState . cars) Car.render
  Toad.render (ent ^. gameState . toad)

renderBackground :: (Renderer m) => m ()
renderBackground = do
  Renderer.drawTileRow ImageAsset.Swamp (V2 0 0    ) 20 (V2 64 64)
  Renderer.drawTileRow ImageAsset.Water (V2 0 1*64 ) 20 (V2 64 64)
  Renderer.drawTileRow ImageAsset.Water (V2 0 2*64 ) 20 (V2 64 64)
  Renderer.drawTileRow ImageAsset.Water (V2 0 3*64 ) 20 (V2 64 64)
  Renderer.drawTileRow ImageAsset.Water (V2 0 4*64 ) 20 (V2 64 64)
  Renderer.drawTileRow ImageAsset.Water (V2 0 5*64 ) 20 (V2 64 64)
  Renderer.drawTileRow ImageAsset.Grass (V2 0 6*64 ) 20 (V2 64 64)
  Renderer.drawTileRow ImageAsset.Road  (V2 0 7*64 ) 20 (V2 64 64)
  Renderer.drawTileRow ImageAsset.Road  (V2 0 8*64 ) 20 (V2 64 64)
  Renderer.drawTileRow ImageAsset.Road  (V2 0 9*64 ) 20 (V2 64 64)
  Renderer.drawTileRow ImageAsset.Road  (V2 0 10*64) 20 (V2 64 64)
  Renderer.drawTileRow ImageAsset.Grass (V2 0 11*64) 20 (V2 64 64)
