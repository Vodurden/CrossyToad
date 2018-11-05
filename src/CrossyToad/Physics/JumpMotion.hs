{-# LANGUAGE TemplateHaskell #-}

-- | Describes a method of travel where the entity "jumps" from one position to another.
-- |
-- | Jumps are not instant and can vary in speed and distance. Also: If the entity is already
-- | jumping it will not be able to jump again until it completes the original jump.
module CrossyToad.Physics.JumpMotion where

import Control.Lens
import Control.Monad.State (MonadState)
import Linear.V2

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
stepJumpMotion :: (MonadState s m, HasJumpMotion s) => m (V2 Float)
stepJumpMotion = do
  motion' <- use jumpMotion
  let distance = min (motion' ^. velocity) (motion' ^. targetDistance)
  let directionVector = unitVector $ motion' ^. direction
  let motionVector = (* distance) <$> directionVector

  jumpMotion.targetDistance .= (motion' ^. targetDistance) - distance
  pure motionVector
