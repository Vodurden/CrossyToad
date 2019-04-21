{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module CrossyToad.Game.SpawnPoint
  ( SpawnPoint(..)
  , HasSpawnPoint(..)
  , HasSpawnPoints(..)
  , SpawnData(..)
  , mk
  , mkUniform
  , tickAll
  , tick
  ) where

import           Control.Lens.Extended
import           Control.Monad.State.Strict (State)
import           Data.Maybe (maybeToList)
import           Data.Foldable (foldl')

import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Direction (Direction, HasDirection(..))
import           CrossyToad.Physics.Speed (Speed, HasSpeed(..))
import           CrossyToad.Game.Entity (Entity)
import           CrossyToad.Game.Command (Command(..))
import           CrossyToad.Time.Seconds (Seconds)
import           CrossyToad.Time.Timed (Timed)
import qualified CrossyToad.Time.Timed as Timed

data SpawnPoint = SpawnPoint
  { __position :: !Position      -- ^ Position to spawn at
  , __direction :: !Direction    -- ^ Direction the spawned entity should face
  , _spawns :: !(Timed (Maybe SpawnData))   -- ^ When to spawn the given entity, and at what speed
  } deriving (Eq, Show)

data SpawnData = SpawnData
  { _entity :: Entity
  , __speed :: Speed
  } deriving (Eq, Show)

makeClassy ''SpawnPoint
makeClassy ''SpawnData

class HasSpawnPoints a where
  spawnPoints :: Lens' a [SpawnPoint]

instance HasSpawnPoints [SpawnPoint] where spawnPoints = id
instance HasPosition SpawnPoint where position = _position
instance HasDirection SpawnPoint where direction = _direction

instance HasSpeed SpawnData where speed = _speed

mk :: Position -> Direction -> Seconds -> [(Seconds, SpawnData)] -> SpawnPoint
mk position' direction' loopInterval spawnTimes = SpawnPoint
    { __position = position'
    , __direction = direction'
    , _spawns =
      foldl' mkSpawn (Timed.mk Nothing) spawnTimes
        & (Timed.after loopInterval Nothing)
        & Timed.loop
    }
  where
    mkSpawn :: Timed (Maybe SpawnData) -> (Seconds, SpawnData) -> Timed (Maybe SpawnData)
    mkSpawn t (seconds, ent) = Timed.pulse seconds ent t

-- | Create a spawner that spawns the same vehicle type at the same speed
mkUniform :: Entity -> Position -> Direction -> Speed -> Int -> Seconds -> Seconds -> SpawnPoint
mkUniform entity' position' direction' speed' spawnCount spawnInterval' loopInterval' =
  let spawnTimes = take spawnCount $ iterate (const spawnInterval') 0
      spawnData' = (, SpawnData entity' speed') <$> spawnTimes
  in mk position' direction' loopInterval' spawnData'

tickAll :: HasSpawnPoints ent => Seconds -> State ent [Command]
tickAll seconds = do
  zoom (spawnPoints.traverse) (maybeToList <$> tick seconds)

tick :: HasSpawnPoint ent => Seconds -> State ent (Maybe Command)
tick seconds = do
  pos <- use (spawnPoint.position)
  dir <- use (spawnPoint.direction)

  spawnData' <- zoom spawns (Timed.tick seconds)
  let entity' = (view entity) <$> spawnData'
  let speed' = (view speed) <$> spawnData'

  pure $ Spawn <$> entity' <*> (pure pos) <*> (pure dir) <*> speed'
