{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.Toad where

import Control.Lens
import Linear.V2

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

-- | How many seconds it takes for the toad to jump once
speed :: Speed
speed = 0.8

-- ^ How far the toad moves in one jump
distance :: Distance
distance = 32

step :: Toad -> Toad
step = toad.body %~ stepBody

-- | Jump in a given direction.
-- |
-- | This will cause the toad to change direction and begin moving.
jump :: Direction -> Toad -> Toad
jump dir =
  over (toad.body.jumpMotion) $ (direction .~ dir)
                              . (velocity .~ speed)
                              . (targetDistance .~ distance)
