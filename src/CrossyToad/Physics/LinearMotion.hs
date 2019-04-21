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
  , mk
  , tick
  , motionVectorThisTick
  ) where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Geometry.Position (HasPosition(..))
import           CrossyToad.Physics.Direction (HasDirection(..))
import qualified CrossyToad.Physics.Direction as Direction
import           CrossyToad.Physics.Distance (Distance)
import           CrossyToad.Physics.Speed (Speed, HasSpeed(..))
import           CrossyToad.Time.Seconds

data LinearMotion = LinearMotion
  { __speed     :: !Speed     -- ^ How fast we are moving
  } deriving (Eq, Show)

makeClassy ''LinearMotion

instance HasSpeed LinearMotion where speed = _speed

mk :: Speed -> LinearMotion
mk = LinearMotion

-- | Step this motion by a given amount of seconds
tick ::
  ( HasPosition ent
  , HasDirection ent
  , HasLinearMotion ent
  ) => Seconds -> ent -> ent
tick delta ent' =
  ent' & position +~ (motionVectorThisTick delta ent')

motionVectorThisTick ::
  ( HasDirection ent
  , HasLinearMotion ent
  ) => Seconds -> ent -> V2 Distance
motionVectorThisTick delta ent' =
  let distanceThisFrame = (ent' ^. linearMotion.speed) * delta
      directionVector = Direction.unitVector $ ent'^.direction
  in (* distanceThisFrame) <$> directionVector
