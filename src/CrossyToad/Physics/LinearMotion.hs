{-# LANGUAGE TemplateHaskell #-}

-- | Describes a method of travel where the entity moves at a constant speed
-- | in a single direction.
-- |
-- | This module is designed to be imported qualified:
-- |
-- |    import           CrossyToad.Physics.LinearMotion (LinearMotion(..), HasLinearMotion(..))
-- |    import qualified CrossyToad.Physics.LinearMotion as LinearMotion
-- |
module CrossyToad.Physics.LinearMotion
  ( LinearMotion(..)
  , HasLinearMotion(..)
  , initialLinearMotion
  , stepEff
  , step
  ) where

import Control.Lens

import CrossyToad.Time.Time
import CrossyToad.Physics.Direction
import CrossyToad.Physics.Speed
import CrossyToad.Physics.Position

data LinearMotion = LinearMotion
  { __direction :: Direction -- ^ The direction we are moving
  , _speed     :: Speed     -- ^ How fast we are moving
  } deriving (Eq, Show)

makeClassy ''LinearMotion

instance HasDirection LinearMotion where
  direction = _direction

initialLinearMotion :: LinearMotion
initialLinearMotion = LinearMotion
  { __direction = East
  , _speed = 0
  }

stepEff :: (Time m, HasPosition ent, HasLinearMotion ent) => ent -> m ent
stepEff ent' = do
  delta <- deltaTime
  pure $ step delta ent'

step :: (HasPosition ent, HasLinearMotion ent) => Seconds -> ent -> ent
step delta ent' =
  let distanceThisFrame = (ent' ^. speed) * delta
      directionVector = unitVector $ ent'^.linearMotion.direction
      motionVector' = (* distanceThisFrame) <$> directionVector
  in ent' & (position +~ motionVector')
