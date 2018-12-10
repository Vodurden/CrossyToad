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
  , step
  , stepBy
  ) where

import Control.Lens

import CrossyToad.Effect.Time.Time
import CrossyToad.Geometry.Position
import CrossyToad.Physics.Direction
import CrossyToad.Physics.Speed

data LinearMotion = LinearMotion
  { _speed     :: !Speed     -- ^ How fast we are moving
  } deriving (Eq, Show)

makeClassy ''LinearMotion

mk :: Speed -> LinearMotion
mk = LinearMotion

step ::
  ( Time m
  , HasPosition ent
  , HasDirection ent
  , HasLinearMotion ent
  ) => ent -> m ent
step ent = do
  delta <- deltaTime
  pure $ stepBy delta ent

-- | Step this motion by a given amount of seconds
-- |
-- | TODO: Replace all usages of `float` with `int` and upscale
-- |       where appropriate
stepBy ::
  ( HasPosition ent
  , HasDirection ent
  , HasLinearMotion ent
  ) => Seconds -> ent -> ent
stepBy delta ent' =
  let distanceThisFrame = (ent' ^. speed) * delta
      directionVector = unitVector $ ent'^.direction
      motionVector' = (* distanceThisFrame) <$> directionVector
  in ent' & (position +~ motionVector')
