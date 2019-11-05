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

import           CrossyToad.Game.GameState (GameState, HasGameState(..), crocHeads, crocBodies)
import qualified CrossyToad.Game.GameState as GameState
import           CrossyToad.Game.Toad (HasToad(..))
import qualified CrossyToad.Game.Toad as Toad
import           CrossyToad.Input.Intent (Intent(..))
import           CrossyToad.Input.Intents (Intents)
import qualified CrossyToad.Input.Intents as Intents
import           CrossyToad.Logger.MonadLogger (MonadLogger(..))
import           CrossyToad.Mortality.Mortal (HasMortal(..))
import qualified CrossyToad.Mortality.MortalSystem as MortalSystem
import qualified CrossyToad.Physics.MovementSystem as MovementSystem
-- import qualified CrossyToad.Physics.Rendering as PhysicsRendering
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
import           CrossyToad.Stage.MonadStage (MonadStage)
import qualified CrossyToad.Stage.MonadStage as MonadStage
import           CrossyToad.Time.Seconds (Seconds)
import           CrossyToad.Victory.Score (HasScore(..))
import qualified CrossyToad.Victory.VictorySystem as VictorySystem

scene ::
  ( MonadRenderer m
  , MonadScene m
  , MonadLogger m
  , MonadStage m
  ) => m (Scene m)
scene = Scene.load initialize handleInput tick render

-- Note: The game is on a 20 by 15 board. With (0,0) as the top left corner and (20,15)
--       as the bottom right.
initialize :: (MonadStage m) => m GameState
initialize = do
  firstStage <- head <$> MonadStage.stages
  pure $ GameState.fromStage firstStage

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

tick :: (MonadScene m, MonadLogger m, HasGameState ent) => Seconds -> ent -> m ent
tick seconds ent' = tickState seconds ent' >>= tickSceneChange

tickState :: (MonadLogger m, HasGameState ent) => Seconds -> ent -> m ent
tickState seconds ent' = flip execStateT ent' $ do
  -- Toad Physics
  gameState.toad %= MovementSystem.tickJumping seconds
  gameState %= lensFoldl' (MovementSystem.moveOnPlatform $ seconds) toad woodLogs
  gameState %= lensFoldl' (MovementSystem.moveOnPlatform $ seconds) toad turtles
  gameState %= lensFoldl' (MovementSystem.moveOnPlatform $ seconds) toad divingTurtles
  gameState %= lensFoldl' (MovementSystem.moveOnPlatform $ seconds) toad crocHeads
  gameState %= lensFoldl' (MovementSystem.moveOnPlatform $ seconds) toad crocBodies

  -- Vehicle Physics
  gameState.cars.mapped %= (MovementSystem.tickLinear seconds)
  gameState.sportsCars.mapped %= (MovementSystem.tickLinear seconds)
  gameState.farmTractors.mapped %= (MovementSystem.tickLinear seconds)
  gameState.trucks.mapped %= (MovementSystem.tickLinear seconds)
  gameState.grassSnakes.mapped %= (MovementSystem.tickLinear seconds)
  gameState.woodLogs.mapped %= (MovementSystem.tickLinear seconds)
  gameState.turtles.mapped %= (MovementSystem.tickLinear seconds)
  gameState.divingTurtles.mapped %= (MovementSystem.tickLinear seconds)
  gameState.crocs.mapped %= (MovementSystem.tickLinear seconds)

  -- Submersible Physics
  gameState.divingTurtles.mapped %= (MovementSystem.tickSubmersible seconds)
  gameState.crocHeads.mapped %= (MovementSystem.tickSubmersible seconds)

  -- Victory
  gameState %= lensMapAccumL (VictorySystem.collectScorable) toad scoreZones
  gameState %= lensMapAccumL (VictorySystem.collectScorable) toad toadHomes
  gameState %= lensMapAccumL (VictorySystem.goalCollision) toad toadHomes

  -- Life & Death
  gameState %= lensFoldl' MortalSystem.mortalCollision toad cars
  gameState %= lensFoldl' MortalSystem.mortalCollision toad sportsCars
  gameState %= lensFoldl' MortalSystem.mortalCollision toad farmTractors
  gameState %= lensFoldl' MortalSystem.mortalCollision toad grassSnakes
  gameState %= lensFoldl' MortalSystem.mortalCollision toad trucks
  gameState %= lensFoldl' MortalSystem.mortalCollision toad deathTerrain

  -- Animation
  gameState.toad %= AnimationSystem.tickToadAnimation seconds
  gameState.divingTurtles.mapped %= (AnimationSystem.tickTurtleAnimation seconds)
  gameState.toadHomes.mapped %= (AnimationSystem.tickToadHomeAnimation seconds)
  gameState.crocHeads.mapped %= (AnimationSystem.tickCrocAnimation seconds)

-- | Scene change must be done separately from `tickState` as we can't easily combine `StateT` and `MonadScene` due to
-- | the lack of an easy MonadTrans instance
tickSceneChange :: (MonadScene m, HasGameState ent) => ent -> m ent
tickSceneChange ent' = do
  MortalSystem.checkGameOver (ent' ^. gameState . toad)
  pure $ ent'

render :: (MonadRenderer m, HasGameState ent) => ent -> m ()
render ent = do
  MonadRenderer.clearScreen

  -- Background
  sequence_ $ concat
    [ Animated.renderNoDirection <$> (ent ^. gameState . deathTerrain)
    , Animated.renderNoDirection <$> (ent ^. gameState . safeTerrain)
    ]

  -- Vehicles
  sequence_ $ concat
    [ Animated.render <$> (ent ^. gameState . cars)
    , Animated.render <$> (ent ^. gameState . sportsCars)
    , Animated.render <$> (ent ^. gameState . farmTractors)
    , Animated.render <$> (ent ^. gameState . grassSnakes)
    , Animated.render <$> (ent ^. gameState . trucks)
    , Animated.render <$> (ent ^. gameState . divingTurtles)
    , Animated.render <$> (ent ^. gameState . turtles)
    , Animated.render <$> (ent ^. gameState . woodLogs)
    , Animated.render <$> (ent ^. gameState . crocs)
    , Animated.renderNoDirection <$> (ent ^. gameState . toadHomes)
    ]

  -- Player
  Animated.render $ ent^.gameState.toad

  -- Player Stats
  renderScore (ent^.gameState.toad)
  renderLives (ent^.gameState.toad)

  -- Debug Rendering
  -- sequence_ $ MonadRenderer.runRenderCommand <$> concat
  --   [ PhysicsRendering.renderPhysical <$> (ent ^. gameState . woodLogs)
  --   ]

  MonadRenderer.drawScreen

renderScore :: (MonadRenderer m, HasScore ent) => ent -> m ()
renderScore ent =
    MonadRenderer.drawText
      FontAsset.PressStart
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

renderLives :: (MonadRenderer m, HasMortal ent) => ent -> m ()
renderLives ent =
    MonadRenderer.drawText
      FontAsset.PressStart
      Nothing
      Nothing
      (Just livesClip)
      RGBAColour.white
      livesText
  where
    livesPos = V2 (6*64) (14*64)
    livesSize = V2 256 64
    livesClip = Clip.mkAt livesPos livesSize
    livesText = Text.pack $ "Lives: " ++ (show $ ent^.lives)
