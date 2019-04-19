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
  , stepBy
  , motionVectorThisTick
  ) where

import Control.Lens
import Linear.V2

import CrossyToad.Geometry.Position
import CrossyToad.Physics.Direction
import CrossyToad.Physics.Distance
import CrossyToad.Physics.Speed
import CrossyToad.Time.Seconds

data LinearMotion = LinearMotion
  { _speed     :: !Speed     -- ^ How fast we are moving
  } deriving (Eq, Show)

makeClassy ''LinearMotion

mk :: Speed -> LinearMotion
mk = LinearMotion

-- | Step this motion by a given amount of seconds
stepBy ::
  ( HasPosition ent
  , HasDirection ent
  , HasLinearMotion ent
  ) => Seconds -> ent -> ent
stepBy delta ent' =
  ent' & position +~ (motionVectorThisTick delta ent')

motionVectorThisTick ::
  ( HasDirection ent
  , HasLinearMotion ent
  ) => Seconds -> ent -> V2 Distance
motionVectorThisTick delta ent' =
  let distanceThisFrame = (ent' ^. speed) * delta
      directionVector = unitVector $ ent'^.direction
  in (* distanceThisFrame) <$> directionVector
