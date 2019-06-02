{-# LANGUAGE TupleSections #-}

module CrossyToad.Game.Game
  ( scene
  , initialize
  , handleInput
  , tick
  , render
  , module CrossyToad.Game.GameState
  ) where

import           Control.Lens.Extended
import           Control.Monad.State.Strict.Extended (execStateT)
import           Data.Foldable (foldlM)
import qualified Data.Text as Text
import           Linear.V2

import           CrossyToad.Input.Intents (Intents)
import qualified CrossyToad.Input.Intents as Intents
import           CrossyToad.Input.Intent (Intent(..))
import           CrossyToad.Game.GameState (GameState, HasGameState(..), crocHeads, crocBodies)
import qualified CrossyToad.Game.GameState as GameState
import           CrossyToad.Game.Terrain (Terrain)
import qualified CrossyToad.Game.Terrain as Terrain
import           CrossyToad.Game.Toad (HasToad(..))
import qualified CrossyToad.Game.Toad as Toad
import           CrossyToad.Game.ToadHome (ToadHome)
import qualified CrossyToad.Game.ToadHome as ToadHome
import           CrossyToad.Game.Vehicle (Car, Truck, WoodLog)
import qualified CrossyToad.Game.Vehicle as Vehicle
import           CrossyToad.Game.Turtle (Turtle)
import qualified CrossyToad.Game.Turtle as Turtle
import           CrossyToad.Game.Croc (Croc)
import qualified CrossyToad.Game.Croc as Croc
import           CrossyToad.Geometry.Position (fromGrid)
import           CrossyToad.Logger.MonadLogger (MonadLogger(..))
import qualified CrossyToad.Mortality.MortalSystem as MortalSystem
import           CrossyToad.Physics.Direction (Direction(..))
import qualified CrossyToad.Physics.MovementSystem as MovementSystem
import           CrossyToad.Physics.Speed (secondsPerTile)
import qualified CrossyToad.Physics.Rendering as PhysicsRendering
import qualified CrossyToad.Renderer.Animated as Animated
import qualified CrossyToad.Renderer.AnimationSystem as AnimationSystem
import qualified CrossyToad.Renderer.Asset.FontAsset as FontAsset
import qualified CrossyToad.Renderer.Clip as Clip
import           CrossyToad.Renderer.MonadRenderer (MonadRenderer)
import qualified CrossyToad.Renderer.MonadRenderer as MonadRenderer
import qualified CrossyToad.Renderer.RGBAColour as RGBAColour
import           CrossyToad.Scene.MonadScene (MonadScene)
import qualified CrossyToad.Scene.MonadScene as MonadScene
import           CrossyToad.Scene.Scene (Scene)
import qualified CrossyToad.Scene.Scene as Scene
import           CrossyToad.Time.Seconds (Seconds)
import           CrossyToad.Victory.Score (HasScore(..))
import qualified CrossyToad.Victory.VictorySystem as VictorySystem

scene ::
  ( MonadRenderer m
  , MonadScene m
  , MonadLogger m
  ) => Scene m
scene = Scene.mk initialize handleInput tick render

-- Note: The game is on a 20 by 15 board. With (0,0) as the top left corner and (20,15)
--       as the bottom right.
initialize :: GameState
initialize = GameState.mk &
    (gameState.toad .~ Toad.mk (fromGrid 10 13))
    . (gameState.deathTerrain .~ deathTerrain')
    . (gameState.safeTerrain .~ safeTerrain')
    . (gameState.toadHomes .~ toadHomes')
    . (gameState.cars .~ cars')
    . (gameState.trucks .~ trucks')
    . (gameState.turtles .~ turtles')
    . (gameState.crocs .~ crocs')
    . (gameState.woodLogs .~ woodLogs')
  where
    deathTerrain' :: [Terrain]
    deathTerrain' = concat
      [ Terrain.mkSwamp <$> (\x -> fromGrid x 0 ) <$> [0..20]
      , Terrain.mkWater <$> (\x -> fromGrid x 1 ) <$> [0..20]
      , Terrain.mkWater <$> (\x -> fromGrid x 2 ) <$> [0..20]
      , Terrain.mkWater <$> (\x -> fromGrid x 3 ) <$> [0..20]
      , Terrain.mkWater <$> (\x -> fromGrid x 4 ) <$> [0..20]
      , Terrain.mkWater <$> (\x -> fromGrid x 5 ) <$> [0..20]
      ]

    safeTerrain' :: [Terrain]
    safeTerrain' = concat
      [ Terrain.mkGrass <$> (\x -> fromGrid x 6 ) <$> [0..20]
      , Terrain.mkRoad  <$> (\x -> fromGrid x 7 ) <$> [0..20]
      , Terrain.mkRoad  <$> (\x -> fromGrid x 8 ) <$> [0..20]
      , Terrain.mkRoad  <$> (\x -> fromGrid x 9 ) <$> [0..20]
      , Terrain.mkRoad  <$> (\x -> fromGrid x 10) <$> [0..20]
      , Terrain.mkRoad  <$> (\x -> fromGrid x 11) <$> [0..20]
      , Terrain.mkGrass <$> (\x -> fromGrid x 12) <$> [0..20]
      ]

    toadHomes' :: [ToadHome]
    toadHomes' =
      [ ToadHome.mk (fromGrid 2  0)
      , ToadHome.mk (fromGrid 7  0)
      , ToadHome.mk (fromGrid 12 0)
      , ToadHome.mk (fromGrid 17 0)
      ]

    cars' :: [Car]
    cars' = concat
      [ (\x -> Vehicle.mkCar   (fromGrid x 8 ) East (secondsPerTile 0.5)) <$> [0,10]
      , (\x -> Vehicle.mkCar   (fromGrid x 9 ) West (secondsPerTile 1)) <$> [10,13,16,19,2]
      , (\x -> Vehicle.mkCar   (fromGrid x 10) East (secondsPerTile 1)) <$> [12,16,0,4,8]
      , (\x -> Vehicle.mkCar   (fromGrid x 11) West (secondsPerTile 1)) <$> [7,11,15,19,3]
      ]

    trucks' :: [Truck]
    trucks' = concat
      [ (\x -> Vehicle.mkTruck (fromGrid x 6) East (secondsPerTile 0.8)) <$> [10,19]
      , (\x -> Vehicle.mkTruck (fromGrid x 7) West (secondsPerTile 0.8)) <$> [10,19]
      ]

    turtles' :: [Turtle]
    turtles' = concat
      [ (\x -> Turtle.mk (fromGrid x 5) West (secondsPerTile 0.75)) <$> [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19]
      ]

    crocs' :: [Croc]
    crocs' = concat
      [ (\x -> Croc.mk (fromGrid x 1) East (secondsPerTile 1)) <$> [1,6,11,16]
      ]

    woodLogs' :: [WoodLog]
    woodLogs' = concat
      [ (\x -> Vehicle.mkWoodLog (fromGrid x 2) West (secondsPerTile 0.75)) <$> [0,1,4,5,8,9,12,13,16,17]
      , (\x -> Vehicle.mkWoodLog (fromGrid x 3) East (secondsPerTile 0.3)) <$> [0,1,2,3,4,8,9,10,11,12,15,16,17,18,19]
      , (\x -> Vehicle.mkWoodLog (fromGrid x 4) East (secondsPerTile 1)) <$> [0,1,5,6,10,11,15,16]
      ]

-- | Update the GameState and Scene based on the user input
handleInput :: (MonadScene m, HasGameState ent) => Intents -> ent -> m ent
handleInput intents ent' =
    foldlM (flip tickIntent) ent' (Intents.all intents)
  where
    tickIntent :: (MonadScene m, HasGameState ent) => Intent -> ent -> m ent
    tickIntent (Move dir) ent = pure $ ent & gameState . toad %~ (Toad.jump dir)
    tickIntent EnterOrConfirm ent = pure ent
    tickIntent PauseOrExit ent = MonadScene.delayPop >> pure ent
    tickIntent ForceExit ent = MonadScene.delayPop >> pure ent

tick :: (MonadLogger m, HasGameState ent) => Seconds -> ent -> m ent
tick seconds ent' = flip execStateT ent' $ do
  -- Toad Physics
  gameState.toad %= MovementSystem.tickJumping seconds
  gameState %= lensFoldl' (MovementSystem.moveOnPlatform $ seconds) toad woodLogs
  gameState %= lensFoldl' (MovementSystem.moveOnPlatform $ seconds) toad turtles
  gameState %= lensFoldl' (MovementSystem.moveOnPlatform $ seconds) toad crocHeads
  gameState %= lensFoldl' (MovementSystem.moveOnPlatform $ seconds) toad crocBodies

  -- Vehicle Physics
  gameState.cars.mapped %= (MovementSystem.tickLinear seconds)
  gameState.trucks.mapped %= (MovementSystem.tickLinear seconds)
  gameState.woodLogs.mapped %= (MovementSystem.tickLinear seconds)
  gameState.turtles.mapped %= (MovementSystem.tickLinear seconds)
  gameState.crocs.mapped %= (MovementSystem.tickLinear seconds)

  -- Submersible Physics
  gameState.turtles.mapped %= (MovementSystem.tickSubmersible seconds)
  gameState.crocHeads.mapped %= (MovementSystem.tickSubmersible seconds)

  -- Victory
  gameState.toad %= VictorySystem.jumpScore
  gameState %= lensMapAccumL (VictorySystem.collectScorable) toad toadHomes
  gameState %= lensMapAccumL (VictorySystem.goalCollision) toad toadHomes

  -- Life & Death
  gameState %= lensFoldl' MortalSystem.mortalCollision toad cars
  gameState %= lensFoldl' MortalSystem.mortalCollision toad trucks
  gameState %= lensFoldl' MortalSystem.mortalCollision toad deathTerrain

  -- Animation
  gameState.toad %= AnimationSystem.tickToadAnimation seconds
  gameState.turtles.mapped %= (AnimationSystem.tickTurtleAnimation seconds)
  gameState.toadHomes.mapped %= (AnimationSystem.tickToadHomeAnimation seconds)
  gameState.crocHeads.mapped %= (AnimationSystem.tickCrocAnimation seconds)

render :: (MonadRenderer m, HasGameState ent) => ent -> m ()
render ent = do
  MonadRenderer.clearScreen

  -- Background
  sequence_ $ MonadRenderer.runRenderCommand <$> concat
    [ Animated.renderNoDirection <$> (ent ^. gameState . deathTerrain)
    , Animated.renderNoDirection <$> (ent ^. gameState . safeTerrain)
    ]

  -- Vehicles
  sequence_ $ MonadRenderer.runRenderCommand <$> concat
    [ Animated.render <$> (ent ^. gameState . cars)
    , Animated.render <$> (ent ^. gameState . trucks)
    , Animated.render <$> (ent ^. gameState . turtles)
    , Animated.render <$> (ent ^. gameState . woodLogs)
    , Animated.render <$> (ent ^. gameState . crocs)
    , Animated.renderNoDirection <$> (ent ^. gameState . toadHomes)
    ]

  -- Player
  MonadRenderer.runRenderCommand (Animated.render $ ent^.gameState.toad)

  -- Player Stats
  renderScore (ent^.gameState.toad)

  -- Debug Rendering
  sequence_ $ MonadRenderer.runRenderCommand <$> concat
    [ PhysicsRendering.renderPhysical <$> (ent ^. gameState . crocHeads)
    , PhysicsRendering.renderPhysical <$> (ent ^. gameState . crocBodies)
    ]

  MonadRenderer.drawScreen

renderScore :: (MonadRenderer m, HasScore ent) => ent -> m ()
renderScore ent =
    MonadRenderer.drawText
      FontAsset.Title
      Nothing
      Nothing
      (Just scoreClip)
      RGBAColour.white
      scoreText
  where
    scorePos = V2 0 (14*64)
    scoreSize = V2 256 64
    scoreClip = Clip.mkAt scorePos scoreSize
    scoreText = Text.pack $ "Score: " ++ (show $ (ent^.score.totalScore))
