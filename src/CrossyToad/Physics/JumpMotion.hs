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
  , HasJumpMotions(..)
  , mk
  , tick
  , jump
  , isJumping
  , isReady
  , isCoolingDown
  ) where

import           Control.Arrow ((>>>))
import           Control.Lens
import           Control.Monad.Extended (whenM)
import           Control.Monad.State.Strict.Extended (State, execState, gets)
import           Data.Maybe.Extended (isJust, whenJust)

import           CrossyToad.Geometry.Position
import           CrossyToad.Geometry.Offset
import           CrossyToad.Physics.Direction
import           CrossyToad.Physics.Distance
import           CrossyToad.Physics.Speed (Speed, HasSpeed(..))
import           CrossyToad.Time.Timed (Timed, HasTimed(..))
import qualified CrossyToad.Time.Timed as Timed
import           CrossyToad.Time.Seconds

data JumpMotion = JumpMotion
  { __speed :: !Speed                   -- ^ How fast we can move
  , _distance :: !Distance              -- ^ How far we move in a single jump
  , _cooldown :: !Seconds               -- ^ How long to cooldown for after a jump

  , _state :: !(Timed JumpMotionState)    -- ^ What we are current doing
  } deriving (Eq, Show)

data JumpMotionState
  = Ready
  | CoolingDown
  | Jumping Distance
  deriving (Eq, Show)

makeClassy ''JumpMotion
makeClassyPrisms ''JumpMotionState

class HasJumpMotions a where
  jumpMotions :: Traversal' a JumpMotion

instance HasJumpMotions JumpMotion where jumpMotions = jumpMotion
instance HasSpeed JumpMotion where speed = _speed

mk :: Speed -> Distance -> Seconds -> JumpMotion
mk speed' distance' cooldown' = JumpMotion
  { __speed = speed'
  , _distance = distance'
  , _cooldown = cooldown'
  , _state = Timed.mk Ready
  }

-- | Jump towards the given direction.
-- |
-- | If we are already moving this function will change nothing.
-- | If we are currently cooling down this function will change nothing.
jump ::
  ( HasDirection ent
  , HasJumpMotion ent
  ) => Direction -> ent -> ent
jump dir ent | (ent^.jumpMotion.state.value == Ready) =
               ent & jumpMotion.state %~ (Timed.immediate $ Jumping (ent^.jumpMotion.distance))
                   & direction .~ dir
             | otherwise = ent

isReady :: (HasJumpMotion ent) => ent -> Bool
isReady motion = isJust $ motion ^? state . value . _Ready

isJumping :: (HasJumpMotion ent) => ent -> Bool
isJumping motion = isJust $ motion ^? state . value . _Jumping

isCoolingDown :: (HasJumpMotion ent) => ent -> Bool
isCoolingDown motion = isJust $ motion ^? state . value . _CoolingDown

-- | Step this motion by a given amount of seconds
tick :: forall ent.
  ( HasPosition ent
  , HasDirection ent
  , HasJumpMotion ent
  ) => Seconds -> ent -> ent
tick delta = execState stepBy'
  where
    stepBy' :: State ent ()
    stepBy' = do
      state %= Timed.tick_ delta

      -- Move if we have any movement to do
      distanceThisFrame' <- gets (distanceThisFrame delta)
      motionVectorThisFrame' <- gets (motionVectorThisFrame distanceThisFrame')
      whenJust motionVectorThisFrame' $ \motionVector' -> do
        state . value . _Jumping %= \targetDistance -> max 0 (targetDistance - distanceThisFrame')
        position += motionVector'

      -- If we have stopped moving, start cooling down
      whenM (uses (state . value) (== Jumping 0)) $ do
        cooldown' <- use cooldown
        state %= ((Timed.immediate $ CoolingDown) >>> (Timed.after cooldown' Ready))

-- | Calculate how far to travel this frame
distanceThisFrame :: (HasJumpMotion ent) => Seconds -> ent -> Distance
distanceThisFrame delta ent' =
  case (ent'^.state.value) of
    Ready -> 0
    CoolingDown -> 0
    (Jumping targetDistance) ->
      let scaledVelocity = (ent' ^. jumpMotion.speed) * delta
      in min scaledVelocity targetDistance

-- | Calculate the motion vector based on how far we want to travel this frame
motionVectorThisFrame :: (HasDirection ent) => Distance -> ent -> Maybe Offset
motionVectorThisFrame 0 _ = Nothing
motionVectorThisFrame distanceThisFrame' ent' =
  let directionVector = unitVector (ent'^.direction)
  in Just $ (* distanceThisFrame') <$> directionVector
