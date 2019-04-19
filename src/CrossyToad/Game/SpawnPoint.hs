{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.SpawnPoint
  ( SpawnPoint(..)
  , HasSpawnPoint(..)
  , HasSpawnPoints(..)
  , mk
  , stepAll
  , step
  ) where

import           Control.Lens.Extended
import           Control.Monad.State.Strict (State)
import           Data.Maybe (maybeToList)
import           Data.Foldable (foldl')

import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Physics (Direction, HasDirection(..))
import           CrossyToad.Game.Entity (Entity)
import           CrossyToad.Game.Command (Command(..))
import           CrossyToad.Time.Seconds (Seconds)
import           CrossyToad.Time.Timed (Timed)
import qualified CrossyToad.Time.Timed as Timed

data SpawnPoint = SpawnPoint
  { __position :: !Position      -- ^ Position to spawn at
  , __direction :: !Direction    -- ^ Direction the spawned entity should face
  , _spawns :: !(Timed (Maybe Entity))   -- ^ When to spawn the given entity
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

mk :: Position -> Direction -> [(Seconds, Entity)] -> Seconds -> SpawnPoint
mk position' direction' spawnTimes loopInterval = SpawnPoint
    { __position = position'
    , __direction = direction'
    , _spawns =
      foldl' mkSpawn (Timed.mk Nothing) spawnTimes
        & (Timed.after loopInterval Nothing)
        & Timed.loop
    }
  where
    mkSpawn :: Timed (Maybe Entity) -> (Seconds, Entity) -> Timed (Maybe Entity)
    mkSpawn t (seconds, ent) = Timed.pulse seconds ent t

stepAll :: HasSpawnPoints ent => Seconds -> State ent [Command]
stepAll seconds = do
  zoom (spawnPoints.traverse) (maybeToList <$> step seconds)

step :: HasSpawnPoint ent => Seconds -> State ent (Maybe Command)
step seconds = do
  nextSpawn <- zoom spawns (Timed.stepBy seconds)
  pos <- use (spawnPoint.position)
  dir <- use (spawnPoint.direction)

  pure $ Spawn <$> nextSpawn <*> (pure pos) <*> (pure dir)
