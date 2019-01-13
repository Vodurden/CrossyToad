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
import           Control.Monad.State.Extended (State, execState)
import           Linear.V2

import           CrossyToad.Geometry.Position
import           CrossyToad.Geometry.Offset
import           CrossyToad.Physics.Direction
import           CrossyToad.Physics.Distance
import           CrossyToad.Physics.Speed
import           CrossyToad.Effect.Time.Time
import           CrossyToad.Effect.Time.Timed (Timed)
import qualified CrossyToad.Effect.Time.Timed as Timed

data JumpMotion = JumpMotion
  { _speed :: Speed              -- ^ How fast we can move
  , _distance :: Distance        -- ^ How far we move in a single jump
  , _cooldown :: Seconds         -- ^ How long to cooldown for after a jump

  , _state :: JumpMotionState    -- ^ What we are current doing
  } deriving (Eq, Show)

data JumpMotionState
  = Ready
  | Jumping Distance
  | CoolingDown Seconds
  deriving (Eq, Show)

makeClassy ''JumpMotion
makeClassyPrisms ''JumpMotionState

mk :: Speed -> Distance -> Seconds -> JumpMotion
mk speed' distance' cooldown' = JumpMotion
  { _speed = speed'
  , _distance = distance'
  , _cooldown = cooldown'
  , _state = Ready
  }

-- | Readies the entity to begin another jump.
ready :: (HasJumpMotion ent) => ent -> ent
ready = state .~ Ready

-- | Jump towards the given direction.
-- |
-- | If we are already moving this function will change nothing.
-- | If we are currently cooling down this function will change nothing.
jump ::
  ( HasDirection ent
  , HasJumpMotion ent
  ) => Direction -> ent -> ent
jump dir ent | (ent^.jumpMotion.state == Ready) =
               ent & jumpMotion.state .~ Jumping (ent^.jumpMotion.distance)
                   & direction .~ dir
             | otherwise = ent

-- | Puts the entity into a cooling down state. The entity will not be able to
-- | jump until the cooldown has finished.
cooldown :: (HasJumpMotion ent) => ent -> ent
cooldown ent =
  ent & state .~ CoolingDown $ Timed.mk (ent^.cooldown) 


step :: (Time m, HasPosition ent, HasDirection ent, HasJumpMotion ent) => ent -> m ent
step ent = do
  delta <- deltaTime
  pure $ stepBy delta ent

-- | Step this motion by a given amount of seconds
stepBy :: (HasPosition ent, HasDirection ent, HasJumpMotion ent) => Seconds -> ent -> ent
stepBy delta ent =
  case ent^.state of
    Ready -> ent
    (Jumping targetDistance) -> ent
    (CoolingDown timed) ->
      ent & state . _CoolingDown .~ Timed.tickBy_ delta

  -- execState $ do
  -- state . _CoolingDown %= Timed.tickBy_ delta
  -- motionVector' <- stepJump delta
  -- position %= (+motionVector')

-- | Steps the cooldown for this frame and returns any remaining delta time.
-- |
-- | The idea is that if the cooldown consumes some of the delta time, the remaining
-- | delta time is still available to the entity to make a short jump.
-- stepCooldown :: (HasJumpMotion s) => Seconds -> State s Seconds
-- stepCooldown delta = do
--   zoom cooldown $ Timed.tickBy_ delta

stepJump :: (HasDirection s, HasJumpMotion s) => Seconds -> State s Offset
stepJump delta = do
  motion' <- use jumpMotion
  direction' <- use direction

  -- TODO: Figure out how to write this better
  if (isMoving motion')
    then do
      let (motionVector', distanceThisFrame) = motionVectorOverTime delta direction' motion'
      let nextDistance = max 0 (motion' ^. targetDistance) - distanceThisFrame

      jumpMotion.targetDistance .= nextDistance

      when (nextDistance == 0) stepMovementFinished

      pure motionVector'
    else
      pure (V2 0 0)

-- | Steps to run when a jump finishes
stepMovementFinished :: (HasJumpMotion s) => State s ()
stepMovementFinished =
  jumpMotion.cooldown %= Timed.start

-- | Calculate the motion vector and distance to travel from the current
-- | motion.
motionVectorOverTime :: Seconds -> Direction -> JumpMotion -> (Offset, Distance)
motionVectorOverTime delta direction' motion' =
  let scaledVelocity = (motion' ^. speed) * delta
      distanceThisFrame = min (scaledVelocity) (motion' ^. targetDistance)
      directionVector = unitVector direction'
      motionVector' = (* distanceThisFrame) <$> directionVector
  in (motionVector', distanceThisFrame)


isCoolingDown :: JumpMotion -> Bool
isCoolingDown motion = Timed.value (motion ^. cooldown) == CoolingDown

isMoving :: (HasJumpMotion ent) => ent -> Bool
isMoving motion = (motion^.targetDistance > 0)
