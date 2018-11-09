{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.Toad where

import Control.Lens
import Linear.V2

import CrossyToad.Time.Time
import CrossyToad.Physics.Physics
import qualified CrossyToad.Physics.JumpMotion as JumpMotion


data Toad = Toad
  { __body :: Body
  } deriving (Eq, Show)

makeClassy ''Toad

instance HasBody Toad where
  body = _body

initialToad :: Toad
initialToad = Toad
    { __body = Body
      { __position = (V2 0 0)
      , __jumpMotion = initialJumpMotion
        { __direction = North
        , _speed = toadSpeed
        , _distance = toadDistance
        , _cooldown = toadCooldown
        }
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
step toad' = do
  nextBody <- stepBody (toad'^.body)
  pure $ (toad.body .~ nextBody) toad'

-- | Jump in a given direction.
-- |
-- | This will cause the toad to change direction and begin moving.
jump :: Direction -> Toad -> Toad
jump dir =
  over (toad.body.jumpMotion) $ (JumpMotion.jump dir)
