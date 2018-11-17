{-# LANGUAGE TemplateHaskell #-}

-- | A lane represents a single row of tiles in the game. It is responsible for spawning
-- | the entities in that lane (Cars, Crocodiles, etc...) and ensuring they are destroyed
-- | when they go off the screen
module CrossyToad.Scene.Game.Lane where

import           Control.Lens
import           Control.Monad ((>=>))
import           Control.Monad.State (MonadState, execStateT)
import           Linear.V2

import           CrossyToad.Physics.Physics
import           CrossyToad.Scene.Game.Car (Car)
import qualified CrossyToad.Scene.Game.Car as Car
import           CrossyToad.Time.Time
import           CrossyToad.Time.Timer (Timer)
import qualified CrossyToad.Time.Timer as Timer

data SpawnScriptOp
  = Spawn
  | Wait Timer
  deriving (Show, Eq)

type SpawnScript = [SpawnScriptOp]

data Lane = Lane
  { __position :: Position
  , _entitySpeed :: Speed
  , _entityDirection :: Direction
  , _spawnedEntities :: [Car]
  , _spawnScript :: SpawnScript
  } deriving (Show, Eq)

makeClassy ''Lane

mk :: Position -> Speed -> Direction -> SpawnScript
mk = undefined

stepEff :: (Time m, MonadState s m, HasLane s) => m ()
stepEff = do
  lane' <- use lane

  nextLane <- stepCars lane'
  lane .= nextLane

step :: (Time m) => Lane -> m Lane
step = stepCars >=> stepSpawnScript

stepCars :: (Time m) => Lane -> m Lane
stepCars lane' = do
  nextCars <- traverse Car.step (lane'^.spawnedEntities)
  pure $ lane' & spawnedEntities .~ nextCars

stepSpawnScript :: (Time m) => Lane -> m Lane
stepSpawnScript lane' = do
  case lane' ^. spawnScript of
    [] -> pure lane'
    (op' : ops) -> do
      -- Run the op
      nextLane <- case op' of
        Spawn -> pure $ spawnCar lane'
        _ -> pure lane'

      -- Transition to the next op
      nextOps <-
        case op' of
          Spawn -> pure ops
          Wait timer' -> do
            nextTimer <- execStateT Timer.stepEff timer'
            pure $ if Timer.running nextTimer
                      then (Wait nextTimer) : ops
                      else ops

      pure $ nextLane & (spawnScript .~ nextOps)

spawnCar :: Lane -> Lane
spawnCar = (lane.spawnedEntities) %~ ((++) [Car.mk (V2 0 1*64) East])
