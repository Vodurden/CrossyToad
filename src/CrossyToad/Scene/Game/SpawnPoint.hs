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
import           Control.Monad.State.Strict (StateT)
import           Data.Maybe (maybeToList)
import           Data.Foldable (foldl')

import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Physics (Direction, HasDirection(..))
import           CrossyToad.Scene.Game.Entity (Entity)
import           CrossyToad.Scene.Game.Command (Command(..))
import           CrossyToad.Effect.Time.Time (Time, Seconds)
import           CrossyToad.Effect.Time.Timed (Timed)
import qualified CrossyToad.Effect.Time.Timed as Timed

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

stepAll :: forall m ent. (Time m, HasSpawnPoints ent) => StateT ent m [Command]
stepAll = do
  zoom (spawnPoints.traverse) (maybeToList <$> step)

step :: (Time m, HasSpawnPoint ent) => StateT ent m (Maybe Command)
step = do
  nextSpawn <- zoom spawns Timed.step
  pos <- use (spawnPoint.position)
  dir <- use (spawnPoint.direction)

  pure $ Spawn <$> nextSpawn <*> (pure pos) <*> (pure dir)
