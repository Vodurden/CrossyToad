{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.Toad where

import Control.Lens
import Linear.Epsilon (nearZero)
import Linear.V2

import CrossyToad.Scene.Game.Direction (Direction(..), HasDirection(..))
import qualified CrossyToad.Scene.Game.Direction as Direction
import CrossyToad.Scene.Game.Distance
import CrossyToad.Scene.Game.Position (Position, HasPosition(..))
import CrossyToad.Scene.Game.Speed

data Toad = Toad
  { __position :: Position
  , __direction :: Direction
  , _targetDistance :: Distance -- ^ How far the frog has left to go in this jump
  } deriving (Eq, Show)

makeClassy ''Toad

instance HasPosition Toad where
  position = _position

instance HasDirection Toad where
  direction = _direction

initialToad :: Toad
initialToad = Toad
  { __position = (V2 0 0)
  , __direction = North
  , _targetDistance = 0
  }

step :: Toad -> Toad
step = stepMovement

stepMovement :: Toad -> Toad
stepMovement t | not $ nearZero $ t^.targetDistance =
  let directionVector = Direction.unitVector $ t^.direction
      distanceThisFrame = min speed (t^.targetDistance)
      jumpVector = (* distanceThisFrame) <$> directionVector
      remainingDistance = t^.targetDistance - distanceThisFrame
  in t & (position +~ jumpVector)
       . (targetDistance .~ remainingDistance)
stepMovement t = t

-- | How many seconds it takes for the toad to jump once
speed :: Speed
speed = 0.8

-- ^ How far the toad moves in one jump
distance :: Distance
distance = 32

-- | Jump in a given direction.
-- |
-- | This will move the toad and change it's direction.
jump :: Direction -> Toad -> Toad
jump dir =
  (direction .~ dir)
  . (targetDistance .~ distance)

isJumping :: Toad -> Bool
isJumping t = nearZero $ t^.targetDistance
