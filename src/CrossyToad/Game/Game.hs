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
import           Control.Monad.State.Strict.Extended (execStateT, hoistState)
import           Data.Foldable (foldl', foldlM)
import qualified Data.Text as Text
import           Linear.V2

import           CrossyToad.Game.Car (Car)
import qualified CrossyToad.Game.Car as Car
import           CrossyToad.Game.Command (Command(..))
import qualified CrossyToad.Game.Entity as Entity
import           CrossyToad.Game.GameState (GameState, HasGameState(..))
import qualified CrossyToad.Game.GameState as GameState
import           CrossyToad.Game.Intent (Intent(..))
import qualified CrossyToad.Game.Intent as Intent
import           CrossyToad.Game.RiverLog (RiverLog)
import qualified CrossyToad.Game.RiverLog as RiverLog
import           CrossyToad.Game.SpawnPoint (SpawnPoint, HasSpawnPoints(..))
import qualified CrossyToad.Game.SpawnPoint as SpawnPoint
import           CrossyToad.Game.Toad (HasToad(..))
import qualified CrossyToad.Game.Toad as Toad
import           CrossyToad.Game.ToadHome (ToadHome)
import qualified CrossyToad.Game.ToadHome as ToadHome
import           CrossyToad.Geometry.Position (fromGrid)
import           CrossyToad.Input.InputState (InputState)
import           CrossyToad.Logger.MonadLogger (MonadLogger(..))
import qualified CrossyToad.Mortality.MortalSystem as MortalSystem
import           CrossyToad.Physics.Direction (Direction(..))
import qualified CrossyToad.Physics.MovementSystem as MovementSystem
import           CrossyToad.Physics.Speed (secondsPerTile)
import qualified CrossyToad.Renderer.Animated as Animated
import qualified CrossyToad.Renderer.AnimationSystem as AnimationSystem
import qualified CrossyToad.Renderer.Asset.FontAsset as FontAsset
import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset
import qualified CrossyToad.Renderer.Clip as Clip
import           CrossyToad.Renderer.MonadRenderer (MonadRenderer)
import qualified CrossyToad.Renderer.MonadRenderer as MonadRenderer
import qualified CrossyToad.Renderer.RGBAColour as RGBAColour
import qualified CrossyToad.Renderer.Sprite as Sprite
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
    . (gameState.toadHomes .~ toadHomes')
    . (gameState.cars .~ cars')
    . (gameState.riverLogs .~ riverLogs')
    . (gameState.spawnPoints .~ spawnPoints')
  where
    toadHomes' :: [ToadHome]
    toadHomes' =
      [ ToadHome.mk (fromGrid 2  0)
      , ToadHome.mk (fromGrid 7  0)
      , ToadHome.mk (fromGrid 12 0)
      , ToadHome.mk (fromGrid 17 0)
      ]

    cars' :: [Car]
    cars' = concat
      [ (\x -> Car.mkTruck (fromGrid x 6) East (secondsPerTile 0.8)) <$> [10,19]
      , (\x -> Car.mkTruck (fromGrid x 7) West (secondsPerTile 0.8)) <$> [10,19]
      , (\x -> Car.mk (fromGrid x 8 ) East (secondsPerTile 0.4)) <$> [0,10]
      , (\x -> Car.mk (fromGrid x 9 ) West (secondsPerTile 1)) <$> [10,13,16,19,2]
      , (\x -> Car.mk (fromGrid x 10) East (secondsPerTile 1)) <$> [12,16,0,4,8]
      , (\x -> Car.mk (fromGrid x 11) West (secondsPerTile 1)) <$> [7,11,15,19,3]
      ]

    riverLogs' :: [RiverLog]
    riverLogs' = concat
      [ (\x -> RiverLog.mk (fromGrid x 1) East (secondsPerTile 1)) <$> [1]
      ]

    spawnPoints' :: [SpawnPoint]
    spawnPoints' = []
      -- [ -- River Spawns
      --   SpawnPoint.mkUniform Entity.RiverLog (fromGrid (-1) 1) East (secondsPerTile 1) 3 4 6

      --   -- Road Spawns
      -- , SpawnPoint.mkUniform Entity.Car (fromGrid 21    7 ) West (secondsPerTile 0.8) 1 0 4
      -- , SpawnPoint.mkUniform Entity.Car (fromGrid (-1)  8 ) East (secondsPerTile 0.5) 1 0 5
      -- , SpawnPoint.mkUniform Entity.Car (fromGrid 21    9 ) West (secondsPerTile 1) 3 4 6
      -- , SpawnPoint.mkUniform Entity.Car (fromGrid (-1)  10) East (secondsPerTile 1) 3 4 6
      -- , SpawnPoint.mkUniform Entity.Car (fromGrid 21    11) West (secondsPerTile 1) 3 4 6
      -- ]

-- | Update the GameState and Scene based on the user input
handleInput :: (MonadScene m, HasGameState ent) => InputState -> ent -> m ent
handleInput input ent' =
    foldlM (flip tickIntent) ent' (Intent.fromInputState input)
  where
    tickIntent :: (MonadScene m, HasGameState ent) => Intent -> ent -> m ent
    tickIntent (Move dir) ent = pure $ ent & gameState . toad %~ (Toad.jump dir)
    tickIntent Exit ent = MonadScene.delayPop >> pure ent

tick :: (MonadLogger m, HasGameState ent) => Seconds -> ent -> m ent
tick seconds ent' = flip execStateT ent' $ do
  -- Physics
  gameState.toad %= MovementSystem.tickJumping seconds
  gameState %= lensFoldl' (MovementSystem.moveOnPlatform $ seconds) toad riverLogs
  gameState.cars.mapped %= (MovementSystem.tickLinear seconds)
  gameState.riverLogs.mapped %= (MovementSystem.tickLinear seconds)

  -- Victory
  gameState.toad %= VictorySystem.jumpScore
  gameState %= lensMapAccumL (VictorySystem.collectScorable) toad toadHomes
  gameState %= lensMapAccumL (VictorySystem.goalCollision) toad toadHomes

  -- Life & Death
  spCommands <- zoom (gameState.spawnPoints) (hoistState $ SpawnPoint.tickAll seconds)
  gameState %= (runCommands spCommands)
  gameState %= lensFoldl' MortalSystem.mortalCollision toad cars

  -- Animation
  gameState.toad %= AnimationSystem.tickToadSprite seconds
  gameState.toadHomes.mapped %= (AnimationSystem.tickToadHomeSprite seconds)

runCommands :: forall ent. (HasGameState ent) => [Command] -> ent -> ent
runCommands commands ent' = foldl' (flip runCommand) ent' commands
  where runCommand :: Command -> ent -> ent
        runCommand (Spawn Entity.Car pos dir speed) = gameState.cars %~ (Car.mk pos dir speed :)
        runCommand (Spawn Entity.RiverLog pos dir speed) = gameState.riverLogs %~ (RiverLog.mk pos dir speed :)
        runCommand Kill = id

render :: (MonadRenderer m, HasGameState ent) => ent -> m ()
render ent = do
  MonadRenderer.clearScreen

  renderBackground'
  sequence_ $ MonadRenderer.runRenderCommand <$> Sprite.renderNoDirection <$> (ent ^. gameState . cars)
  sequence_ $ MonadRenderer.runRenderCommand <$> Sprite.renderNoDirection <$> (ent ^. gameState . riverLogs)
  sequence_ $ MonadRenderer.runRenderCommand <$> Animated.renderNoDirection  <$> (ent ^. gameState . toadHomes)
  MonadRenderer.runRenderCommand $ Animated.render (ent ^. gameState . toad)

  renderScore (ent^.gameState.toad)

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

renderBackground' :: (MonadRenderer m) => m ()
renderBackground' = do
  MonadRenderer.drawTileRow ImageAsset.Swamp (V2 0 0    ) 20 (V2 64 64)
  MonadRenderer.drawTileRow ImageAsset.Water (V2 0 1*64 ) 20 (V2 64 64)
  MonadRenderer.drawTileRow ImageAsset.Water (V2 0 2*64 ) 20 (V2 64 64)
  MonadRenderer.drawTileRow ImageAsset.Water (V2 0 3*64 ) 20 (V2 64 64)
  MonadRenderer.drawTileRow ImageAsset.Water (V2 0 4*64 ) 20 (V2 64 64)
  MonadRenderer.drawTileRow ImageAsset.Water (V2 0 5*64 ) 20 (V2 64 64)
  MonadRenderer.drawTileRow ImageAsset.Grass (V2 0 6*64 ) 20 (V2 64 64)
  MonadRenderer.drawTileRow ImageAsset.Road  (V2 0 7*64 ) 20 (V2 64 64)
  MonadRenderer.drawTileRow ImageAsset.Road  (V2 0 8*64 ) 20 (V2 64 64)
  MonadRenderer.drawTileRow ImageAsset.Road  (V2 0 9*64 ) 20 (V2 64 64)
  MonadRenderer.drawTileRow ImageAsset.Road  (V2 0 10*64) 20 (V2 64 64)
  MonadRenderer.drawTileRow ImageAsset.Road  (V2 0 11*64) 20 (V2 64 64)
  MonadRenderer.drawTileRow ImageAsset.Grass (V2 0 12*64) 20 (V2 64 64)
