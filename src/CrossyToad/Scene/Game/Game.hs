module CrossyToad.Scene.Game.Game
  ( initialize
  , handleInput
  , step
  , stepIntent
  , render
  , module CrossyToad.Scene.Game.GameState
  ) where

import           Control.Lens.Extended
import           Control.Monad ((>=>))
import           Data.Foldable (foldl')
import           Linear.V2

import           CrossyToad.Physics.Physics (Direction(..))
import           CrossyToad.Effect.Input.Input (InputState)
import           CrossyToad.Effect.Logger.Logger (Logger(..))
import qualified CrossyToad.Effect.Renderer.Renderer as Renderer
import qualified CrossyToad.Effect.Renderer.ImageAsset as ImageAsset
import           CrossyToad.Effect.Renderer.RenderCommand (RenderCommand(..))
import           CrossyToad.Effect.Time.Time (Time(..))
import           CrossyToad.Scene.Internal (HasScene, scene)
import qualified CrossyToad.Scene.Internal as Scene
import           CrossyToad.Scene.Game.Intent (Intent(..))
import qualified CrossyToad.Scene.Game.Intent as Intent
import           CrossyToad.Scene.Game.GameState
import           CrossyToad.Scene.Game.Toad (HasToad(..))
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

-- | Update the GameState and Scene based on the user input
handleInput :: (HasGameState ent, HasScene ent) => InputState -> ent -> ent
handleInput input ent =
  foldl' (flip stepIntent) ent (Intent.fromInputState input)

stepIntent :: (HasGameState ent, HasScene ent) => Intent -> ent -> ent
stepIntent (Move dir) = gameState.toad %~ (Toad.jump dir)
stepIntent Exit = scene .~ Scene.Title

step :: (Logger m, Time m, HasGameState ent) => ent -> m ent
step ent = do
  stepGameState ent

-- | Step all the GameState specific logic
stepGameState :: (Logger m, Time m, HasGameState ent) => ent -> m ent
stepGameState =
  mapMOf gameState $
    Toad.step
    >=> SpawnPoint.stepAll
    >=> Car.stepAll
    >=> Collision.step

render :: (HasGameState ent) => ent -> [RenderCommand]
render ent =
  renderBackground'
  ++ (Car.render <$> (ent ^. gameState . cars))
  ++ [Toad.render (ent ^. gameState . toad)]

renderBackground' :: [RenderCommand]
renderBackground' = concat $
  [ Renderer.drawTileRow ImageAsset.Swamp (V2 0 0    ) 20 (V2 64 64)
  , Renderer.drawTileRow ImageAsset.Water (V2 0 1*64 ) 20 (V2 64 64)
  , Renderer.drawTileRow ImageAsset.Water (V2 0 2*64 ) 20 (V2 64 64)
  , Renderer.drawTileRow ImageAsset.Water (V2 0 3*64 ) 20 (V2 64 64)
  , Renderer.drawTileRow ImageAsset.Water (V2 0 4*64 ) 20 (V2 64 64)
  , Renderer.drawTileRow ImageAsset.Water (V2 0 5*64 ) 20 (V2 64 64)
  , Renderer.drawTileRow ImageAsset.Grass (V2 0 6*64 ) 20 (V2 64 64)
  , Renderer.drawTileRow ImageAsset.Road  (V2 0 7*64 ) 20 (V2 64 64)
  , Renderer.drawTileRow ImageAsset.Road  (V2 0 8*64 ) 20 (V2 64 64)
  , Renderer.drawTileRow ImageAsset.Road  (V2 0 9*64 ) 20 (V2 64 64)
  , Renderer.drawTileRow ImageAsset.Road  (V2 0 10*64) 20 (V2 64 64)
  , Renderer.drawTileRow ImageAsset.Grass (V2 0 11*64) 20 (V2 64 64)
  ]
