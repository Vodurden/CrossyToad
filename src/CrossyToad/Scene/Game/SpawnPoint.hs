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
import           Control.Monad.State.Extended (StateT)
import qualified Data.List.Extended as List

import           CrossyToad.Physics.Physics (Position, HasPosition(..), Direction, HasDirection(..))
import           CrossyToad.Scene.Game.Car (Car(..), HasCars(..))
import qualified CrossyToad.Scene.Game.Car as Car
import           CrossyToad.Time.Time (Time, Seconds)
import           CrossyToad.Time.Timer (Timer)
import qualified CrossyToad.Time.Timer as Timer

data SpawnPoint = SpawnPoint
  { __position :: Position   -- ^ Position to spawn at
  , __direction :: Direction -- ^ Direction the spawned entity should face
  , _spawnTimer :: Timer     -- ^ Interval between spawns
  , _loopTimer :: Timer      -- ^ Interval between loops
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
stepAll :: (Time m, HasSpawnPoints s, HasCars s) => StateT s m ()
stepAll = do
  newCars <- zoom (spawnPoints.traverse) step
  cars %= (++ newCars)

-- | Steps a spawn point and return the cars that should
-- | be spawned (if any)
step :: (Time m, HasSpawnPoint s) => StateT s m [Car]
step = do
  _ <- zoom spawnTimer Timer.step
  _ <- zoom loopTimer Timer.step

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
