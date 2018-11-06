{-# LANGUAGE TemplateHaskell #-}

-- | Describes a method of travel where the entity "jumps" from one position to another.
-- |
-- | Jumps are not instant and can vary in speed and distance. Also: If the entity is already
-- | jumping it will not be able to jump again until it completes the original jump.
module CrossyToad.Physics.JumpMotion
  ( JumpMotion(..)
  , HasJumpMotion(..)
  , stepJumpMotion
  , jump
  , isMoving
  ) where

import Control.Lens
import Control.Monad.State (MonadState)
import Linear.V2

import CrossyToad.Time.Time
import CrossyToad.Physics.Direction
import CrossyToad.Physics.Distance
import CrossyToad.Physics.Speed

data JumpMotion = JumpMotion
  { __direction :: Direction     -- ^ What direction we are facing

  , _speed :: Speed              -- ^ How fast we can move
  , _distance :: Distance        -- ^ How far we move in a single jump

  , _targetDistance :: Distance  -- ^ How far we _are_ moving
  } deriving (Eq, Show)

makeClassy ''JumpMotion

instance HasDirection JumpMotion where
  direction = _direction

-- | Jump towards the direction we are facing. This affects the motion vector
-- | returned by @stepJumpMotion@
-- |
-- | If we are already moving this function will change nothing.
jump :: Direction -> JumpMotion -> JumpMotion
jump dir motion | isMoving motion = motion
                | otherwise = motion & (direction .~ dir)
                                     . (targetDistance .~ motion^.distance)

isMoving :: JumpMotion -> Bool
isMoving motion = (motion^.targetDistance > 0)

-- | Returns the motion vector for this frame and updates the motion.
stepJumpMotion :: (MonadState s m, HasJumpMotion s) => Seconds -> m (V2 Float)
stepJumpMotion delta = do
  motion' <- use jumpMotion
  let (motionVector', distanceThisFrame) = motionVectorOverTime delta motion'

  jumpMotion.targetDistance .= max 0 ((motion' ^. targetDistance) - distanceThisFrame)
  pure motionVector'

-- | Calculate the motion vector and distance to travel from the current
-- | motion.
motionVectorOverTime :: Seconds -> JumpMotion -> (V2 Float, Distance)
motionVectorOverTime delta motion' =
  let scaledVelocity = (motion' ^. speed) * delta
      distanceThisFrame = min (scaledVelocity) (motion' ^. targetDistance)
      directionVector = unitVector $ motion' ^. direction
      motionVector' = (* distanceThisFrame) <$> directionVector
  in (motionVector', distanceThisFrame)
