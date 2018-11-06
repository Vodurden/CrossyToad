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
      , __jumpMotion = JumpMotion
        { __direction = North
        , _speed = toadSpeed
        , _distance = toadDistance
        , _targetDistance = 0
        }
      }
    }
  where
    -- | How far the toad moves in one jump
    toadDistance :: Distance
    toadDistance = 32

    -- | How many pixels the toad moves per-second
    toadSpeed :: Speed
    toadSpeed = toadDistance * (1 / secondsToJump)
      where secondsToJump = 0.1

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
