{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.SpawnPoint
  ( SpawnPoint(..)
  , HasSpawnPoint(..)
  , HasSpawnPoints(..)
  , mk
  , stepAll
  , step
  ) where

import           Control.Lens.Extended
import           Control.Monad.State.Extended (StateT, runStateT)
import qualified Data.List.Extended as List

import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Physics (Direction, HasDirection(..))
import           CrossyToad.Scene.Game.Car (Car(..), HasCars(..))
import qualified CrossyToad.Scene.Game.Car as Car
import           CrossyToad.Effect.Time.Time (Time, Seconds)
import           CrossyToad.Effect.Time.Timer (Timer)
import qualified CrossyToad.Effect.Time.Timer as Timer

data SpawnPoint = SpawnPoint
  { __position :: !Position   -- ^ Position to spawn at
  , __direction :: !Direction -- ^ Direction the spawned entity should face
  , _spawnTimer :: !Timer     -- ^ Interval between spawns
  , _loopTimer :: !Timer      -- ^ Interval between loops
  } deriving (Eq, Show)

makeClassy ''SpawnPoint

class HasSpawnPoints a where
  spawnPoints :: Lens' a [SpawnPoint]

instance HasSpawnPoints [SpawnPoint] where
  spawnPoints = id

instance HasPosition SpawnPoint where
  position = _position

instance HasDirection SpawnPoint where
  direction = _direction

mk :: Position -> Direction -> Seconds -> Seconds -> SpawnPoint
mk position' direction' spawnTime loopTime = SpawnPoint
  { __position = position'
  , __direction = direction'
  , _spawnTimer = Timer.start $ Timer.mk spawnTime
  , _loopTimer = Timer.start $ Timer.mk loopTime
  }

-- | Step all spawn points
-- |
-- | We have [StateT ent m [Car]]
stepAll :: forall ent m. (Time m, HasSpawnPoints ent, HasCars ent) => ent -> m ent
stepAll ent = do
    let sps = ent ^. spawnPoints
    states <- traverse (runStateT step) sps
    let newCars = concatMap fst states
    let newSpawnPoints = fmap snd states
    pure $ ent & cars %~ (++ newCars)
               & spawnPoints .~ newSpawnPoints

-- | Steps a spawn point and return the cars that should
-- | be spawned (if any)
step :: (Time m, HasSpawnPoint ent) => StateT ent m [Car]
step = do
  spawnTimer `modifyingM` Timer.step
  loopTimer `modifyingM` Timer.step

  -- If the spawn timer has finished, spawn a car
  spawnTimerFinished <- uses spawnTimer Timer.finished
  let cars' = if spawnTimerFinished
                 then List.singleton <$> uses spawnPoint spawnCar
                 else pure []

  -- Restart the spawn timer if it has finished
  spawnTimer %= Timer.restart

  cars'

spawnCar :: SpawnPoint -> Car
spawnCar spawnPoint' = Car.mk (spawnPoint' ^. position) (spawnPoint' ^. direction)
