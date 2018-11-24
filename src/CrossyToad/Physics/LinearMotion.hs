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
import Control.Monad.State.Extended (StateT)

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

mk :: Direction -> Speed -> LinearMotion
mk = LinearMotion

step :: (Time m, HasPosition s, HasLinearMotion s) => StateT s m ()
step = do
  delta <- deltaTime
  id %= stepBy delta

-- | Step this motion by a given amount of seconds
stepBy :: (HasPosition ent, HasLinearMotion ent) => Seconds -> ent -> ent
stepBy delta ent' =
  let distanceThisFrame = (ent' ^. speed) * delta
      directionVector = unitVector $ ent'^.linearMotion.direction
      motionVector' = (* distanceThisFrame) <$> directionVector
  in ent' & (position +~ motionVector')
