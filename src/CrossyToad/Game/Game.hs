{-# LANGUAGE TupleSections #-}

module CrossyToad.Game.Game
  ( scene
  , initialize
  , handleInput
  , tick
  , tickIntent
  , render
  , module CrossyToad.Game.GameState
  ) where

import           Control.Lens.Extended
import           Control.Monad.State.Strict.Extended (execStateT, hoistState)
import           Data.Foldable (foldl', foldlM)
import qualified Data.Text as Text
import           Linear.V2

import qualified CrossyToad.Game.Car as Car
import           CrossyToad.Game.Command (Command(..))
import qualified CrossyToad.Game.Entity as Entity
import           CrossyToad.Game.GameState (GameState, HasGameState(..))
import qualified CrossyToad.Game.GameState as GameState
import           CrossyToad.Game.Intent (Intent(..))
import qualified CrossyToad.Game.Intent as Intent
import qualified CrossyToad.Game.RiverLog as RiverLog
import           CrossyToad.Game.SpawnPoint (SpawnPoint, HasSpawnPoints(..))
import qualified CrossyToad.Game.SpawnPoint as SpawnPoint
import           CrossyToad.Game.Toad (HasToad(..))
import qualified CrossyToad.Game.Toad as Toad
import           CrossyToad.Game.ToadHome (ToadHome)
import qualified CrossyToad.Game.ToadHome as ToadHome
import           CrossyToad.Input.InputState (InputState)
import           CrossyToad.Logger.MonadLogger (MonadLogger(..))
import qualified CrossyToad.Mortality.MortalSystem as MortalSystem
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import qualified CrossyToad.Physics.MovementSystem as MovementSystem
import           CrossyToad.Physics.Physics (Direction(..))
import qualified CrossyToad.Renderer.Animated as Animated
import qualified CrossyToad.Renderer.AnimationSystem as AnimationSystem
import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset
import qualified CrossyToad.Renderer.Asset.FontAsset as FontAsset
import           CrossyToad.Renderer.MonadRenderer (MonadRenderer)
import qualified CrossyToad.Renderer.MonadRenderer as MonadRenderer
import qualified CrossyToad.Renderer.RGBAColour as RGBAColour
import qualified CrossyToad.Renderer.Sprite as Sprite
import qualified CrossyToad.Renderer.Clip as Clip
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

initialize :: GameState
initialize = GameState.mk &
    (gameState.toad .~ Toad.mk (V2 (7*64) (13*64)))
    . (gameState.toadHomes .~ toadHomes')
    . (gameState.cars .~ [])
    . (gameState.riverLogs .~ [])
    . (gameState.spawnPoints .~ spawnPoints')
  where
    toadHomes' :: [ToadHome]
    toadHomes' =
      [ ToadHome.mk (V2 (2*64 ) 0)
      , ToadHome.mk (V2 (7*64 ) 0)
      , ToadHome.mk (V2 (12*64) 0)
      , ToadHome.mk (V2 (17*64) 0)
      ]

    spawnPoints' :: [SpawnPoint]
    spawnPoints' =
      [ -- River Spawns
        SpawnPoint.mk (V2 0       (1*64 )) East ((,Entity.RiverLog) <$> [1]) 5
        -- TODO

        -- Road Spawns
      , SpawnPoint.mk (V2 (20*64) (7*64 )) West ((,Entity.Car) <$> [0,1,1]) 2
      , SpawnPoint.mk (V2 (20*64) (8*64 )) West ((,Entity.Car) <$> [0,0.6,0.6,0.6]) 1
      , SpawnPoint.mk (V2 0       (9*64 )) East ((,Entity.Car) <$> [0.5,2]) 3
      , SpawnPoint.mk (V2 (20*64) (10*64)) West ((,Entity.Car) <$> [0.5,2,4]) 3
      ]

-- | Update the GameState and Scene based on the user input
handleInput :: (MonadScene m, HasGameState ent) => InputState -> ent -> m ent
handleInput input ent' =
  foldlM (flip tickIntent) ent' (Intent.fromInputState input)

tickIntent :: (MonadScene m, HasGameState ent) => Intent -> ent -> m ent
tickIntent (Move dir) ent = pure $ ent & gameState . toad %~ (Toad.jump dir)
tickIntent Exit ent = MonadScene.delayPop >> pure ent

tick :: (MonadLogger m, HasGameState ent) => Seconds -> ent -> m ent
tick seconds ent' = flip execStateT ent' $ do
  gameState.toad %= MovementSystem.tickJumping seconds
  gameState.toad %= AnimationSystem.tickToadSprite seconds
  gameState %= lensFoldl' (MovementSystem.moveOnPlatform $ seconds) toad riverLogs
  gameState.toad %= VictorySystem.jumpScore
  gameState %= lensMapAccumL (VictorySystem.collectScorable) toad toadHomes
  gameState %= lensMapAccumL (VictorySystem.goalCollision) toad toadHomes

  gameState.toadHomes.mapped %= (AnimationSystem.tickToadHomeSprite seconds)

  spCommands <- zoom (gameState.spawnPoints) (hoistState $ SpawnPoint.tickAll seconds)
  gameState %= (runCommands spCommands)

  gameState.cars.mapped %= (LinearMotion.tick seconds)
  gameState.riverLogs.mapped %= (LinearMotion.tick seconds)

  gameState %= lensFoldl' MortalSystem.mortalCollision toad cars

runCommands :: forall ent. (HasGameState ent) => [Command] -> ent -> ent
runCommands commands ent' = foldl' (flip runCommand) ent' commands
  where runCommand :: Command -> ent -> ent
        runCommand (Spawn Entity.Car pos dir) = gameState.cars %~ (Car.mk pos dir :)
        runCommand (Spawn Entity.RiverLog pos dir) = gameState.riverLogs %~ (RiverLog.mk pos dir :)
        runCommand Kill = id

render :: (MonadRenderer m, HasGameState ent) => ent -> m ()
render ent = do
  MonadRenderer.clearScreen

  renderBackground'
  sequence_ $ MonadRenderer.runRenderCommand <$> Sprite.render <$> (ent ^. gameState . cars)
  sequence_ $ MonadRenderer.runRenderCommand <$> Sprite.render <$> (ent ^. gameState . riverLogs)
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
