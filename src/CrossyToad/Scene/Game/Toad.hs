{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.Toad where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Physics.JumpMotion (JumpMotion(..), HasJumpMotion(..))
import qualified CrossyToad.Physics.JumpMotion as JumpMotion
import           CrossyToad.Physics.Physics
import           CrossyToad.Time.Time

data Toad = Toad
  { __position :: Position
  , __jumpMotion :: JumpMotion
  } deriving (Eq, Show)

makeClassy ''Toad

instance HasPosition Toad where
  position = _position

instance HasJumpMotion Toad where
  jumpMotion = _jumpMotion

initialToad :: Toad
initialToad = Toad
    { __position = (V2 0 0)
    , __jumpMotion = JumpMotion.initialJumpMotion
      { __direction = North
      , _speed = toadSpeed
      , _distance = toadDistance
      , _cooldown = toadCooldown
      }
    }
  where
    -- | How far the toad moves in one jump
    toadDistance :: Distance
    toadDistance = 64

    -- | How many pixels the toad moves per-second
    toadSpeed :: Speed
    toadSpeed = toadDistance * (1 / secondsToJump)
      where secondsToJump = 0.15

    -- | How long the toad must rest between jumps
    toadCooldown :: Seconds
    toadCooldown = 0.15

step :: (Time m) => Toad -> m Toad
step = JumpMotion.stepEff

-- | Jump in a given direction.
-- |
-- | This will cause the toad to change direction and begin moving.
jump :: Direction -> Toad -> Toad
jump dir = over (toad.jumpMotion) $ (JumpMotion.jump dir)
