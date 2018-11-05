{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.Toad where

import Control.Lens
import Linear.V2

import CrossyToad.Time.Time
import CrossyToad.Physics.Physics

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
    , __jumpMotion = JumpMotion
      { __direction = North
      , _velocity = 0
      , _targetDistance = 0
      }
    }
  }

-- | How many pixels the toad moves in a second.
-- speed :: Speed
-- speed = distance * (1 / secondsToJump)
--   where secondsToJump = 0.1
speed :: Speed
speed = 32 * 10

-- ^ How far the toad moves in one jump
distance :: Distance
distance = 32

step :: (Time m) => Toad -> m Toad
step toad' = do
  nextBody <- stepBody (toad'^.body)
  pure $ (toad.body .~ nextBody) toad'

-- | Jump in a given direction.
-- |
-- | This will cause the toad to change direction and begin moving.
jump :: Direction -> Toad -> Toad
jump dir =
  over (toad.body.jumpMotion) $ (direction .~ dir)
                              . (velocity .~ speed)
                              . (targetDistance .~ distance)
