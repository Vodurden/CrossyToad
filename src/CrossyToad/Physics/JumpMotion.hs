{-# LANGUAGE TemplateHaskell #-}

-- | Describes a method of travel where the entity "jumps" from one position to another.
-- |
-- | Jumps are not instant and can vary in speed and distance. Also: If the entity is already
-- | jumping it will not be able to jump again until it completes the original jump.
-- |
-- | This module is designed to be imported qualified:
-- |
-- |    import           CrossyToad.Physics.JumpMotion (JumpMotion(..), HasJumpMotion(..))
-- |    import qualified CrossyToad.Physics.JumpMotion as JumpMotion
-- |
module CrossyToad.Physics.JumpMotion
  ( JumpMotion(..)
  , HasJumpMotion(..)
  , mk
  , step
  , stepBy
  , jump
  , isMoving
  ) where

import           Control.Lens
import           Control.Monad (when)
import           Control.Monad.State.Extended (StateT, State, execState)
import           Linear.V2

import           CrossyToad.Physics.Direction
import           CrossyToad.Physics.Distance
import           CrossyToad.Physics.Position
import           CrossyToad.Physics.Speed
import           CrossyToad.Time.Time
import           CrossyToad.Time.Timer (Timer)
import qualified CrossyToad.Time.Timer as Timer

data JumpMotion = JumpMotion
  { __direction :: Direction     -- ^ What direction we are facing

  , _speed :: Speed              -- ^ How fast we can move
  , _distance :: Distance        -- ^ How far we move in a single jump
  , _cooldown :: Timer           -- ^ Timer to wait between jumps

  , _targetDistance :: Distance  -- ^ How far we _are_ moving
  } deriving (Eq, Show)

makeClassy ''JumpMotion

instance HasDirection JumpMotion where
  direction = _direction

mk :: Direction -> Speed -> Distance -> Seconds -> JumpMotion
mk dir speed' distance' cooldown' = JumpMotion
  { __direction = dir
  , _speed = speed'
  , _distance = distance'
  , _cooldown = Timer.mk cooldown'
  , _targetDistance = 0
  }

-- | Updates the jump motion for this frame and updates the position of the entity.
step :: (Time m, HasPosition s, HasJumpMotion s) => StateT s m ()
step = do
  delta <- deltaTime
  id %= stepBy delta

-- | Step this motion by a given amount of seconds
stepBy :: (HasPosition ent, HasJumpMotion ent) => Seconds -> ent -> ent
stepBy delta = execState $ do
  jumpDelta <- stepCooldown delta
  motionVector' <- stepJump jumpDelta
  position %= (+motionVector')

-- | Steps the cooldown for this frame and returns any remaining delta time.
-- |
-- | The idea is that if the cooldown consumes some of the delta time, the remaining
-- | delta time is still available to the entity to make a short jump.
stepCooldown :: (HasJumpMotion s) => Seconds -> State s Seconds
stepCooldown delta = do
  remainingDelta <- zoom cooldown $ Timer.stepBy delta
  pure remainingDelta

stepJump :: (HasJumpMotion s) => Seconds -> State s (V2 Float)
stepJump delta = do
  motion' <- use jumpMotion

  -- TODO: Figure out how to write this better
  if (isMoving motion')
    then do
      let (motionVector', distanceThisFrame) = motionVectorOverTime delta motion'
      let nextDistance = max 0 (motion' ^. targetDistance) - distanceThisFrame

      jumpMotion.targetDistance .= nextDistance

      when (nextDistance == 0) stepMovementFinished

      pure motionVector'
    else
      pure (V2 0 0)

-- | Steps to run when a jump finishes
stepMovementFinished :: (HasJumpMotion s) => State s ()
stepMovementFinished =
  jumpMotion.cooldown %= Timer.start

-- | Calculate the motion vector and distance to travel from the current
-- | motion.
motionVectorOverTime :: Seconds -> JumpMotion -> (V2 Float, Distance)
motionVectorOverTime delta motion' =
  let scaledVelocity = (motion' ^. speed) * delta
      distanceThisFrame = min (scaledVelocity) (motion' ^. targetDistance)
      directionVector = unitVector $ motion' ^. direction
      motionVector' = (* distanceThisFrame) <$> directionVector
  in (motionVector', distanceThisFrame)

-- |
-- | Jump towards the direction we are facing. This affects the motion vector
-- | returned by @stepJumpMotion@
-- |
-- | If we are already moving this function will change nothing.
-- | If we are currently cooling down this function will change nothing.
jump :: Direction -> JumpMotion -> JumpMotion
jump dir motion | canJump motion = motion & (direction .~ dir)
                                          . (targetDistance .~ motion^.distance)
                | otherwise = motion

canJump :: JumpMotion -> Bool
canJump motion = (not $ isCoolingDown motion) && (not $ isMoving motion)

isCoolingDown :: JumpMotion -> Bool
isCoolingDown motion = Timer.running (motion ^. cooldown)

isMoving :: (HasJumpMotion ent) => ent -> Bool
isMoving motion = (motion^.targetDistance > 0)
