{-# LANGUAGE TemplateHaskell #-}

-- | Describes a method of travel where the entity "jumps" from one position to another.
-- |
-- | Jumps are not instant and can vary in speed and distance. Also: If the entity is already
-- | jumping it will not be able to jump again until it completes the original jump.
module CrossyToad.Physics.JumpMotion
  ( JumpMotion(..)
  , HasJumpMotion(..)
  , stepJumpMotion
  ) where

import Control.Lens
import Control.Monad.State (MonadState)
import Linear.V2

import CrossyToad.Time.Time
import CrossyToad.Physics.Direction
import CrossyToad.Physics.Distance
import CrossyToad.Physics.Speed

data JumpMotion = JumpMotion
  { __direction :: Direction
  , _velocity :: Speed
  , _targetDistance :: Distance
  } deriving (Eq, Show)

makeClassy ''JumpMotion

instance HasDirection JumpMotion where
  direction = _direction

-- | Returns the motion vector for this frame and updates the motion.
stepJumpMotion :: (MonadState s m, HasJumpMotion s) => Seconds -> m (V2 Float)
stepJumpMotion delta = do
  motion' <- use jumpMotion
  let (motionVector', distance) = motionVectorOverTime delta motion'

  jumpMotion.targetDistance .= max 0 ((motion' ^. targetDistance) - distance)
  pure motionVector'

-- | Calculate the motion vector and distance to travel from the current
-- | motion.
motionVectorOverTime :: Seconds -> JumpMotion -> (V2 Float, Distance)
motionVectorOverTime delta motion' =
  let scaledVelocity = (motion' ^. velocity) * delta
      distance = min (scaledVelocity) (motion' ^. targetDistance)
      directionVector = unitVector $ motion' ^. direction
      motionVector' = (* distance) <$> directionVector
  in (motionVector', distance)
