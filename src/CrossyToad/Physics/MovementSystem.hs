{-# LANGUAGE RankNTypes #-}

-- | This module contains all logic for moving entities around
module CrossyToad.Physics.MovementSystem
  ( moveOnAllPlatforms
  , moveOnPlatform
  ) where

import           Control.Lens
import           Data.Foldable (foldl')

import qualified CrossyToad.Geometry.AABB as AABB
import           CrossyToad.Geometry.Position (HasPosition(..))
import           CrossyToad.Physics.CollisionBox (HasCollisionBox(..))
import           CrossyToad.Physics.Direction (HasDirection)
import           CrossyToad.Physics.LinearMotion (HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Physics.Platform (HasPlatform(..))
import           CrossyToad.Time.Seconds (Seconds)

-- | Move the rider by the motion of _all_ platforms they are colliding with
moveOnAllPlatforms ::
  ( HasPosition riderEnt
  , HasCollisionBox riderEnt

  , HasPosition platformEnt
  , HasDirection platformEnt
  , HasPlatform platformEnt
  , HasLinearMotion platformEnt
  ) => Seconds -> Lens' s riderEnt -> Lens' s [platformEnt] -> s -> s
moveOnAllPlatforms delta riderL platformsL state =
  state & riderL .~ foldl' (moveOnPlatform delta) (state ^. riderL) (state ^. platformsL)

-- | Move the rider by the motion of the platform if it is
-- | standing on the platform
moveOnPlatform ::
  ( HasPosition riderEnt
  , HasCollisionBox riderEnt

  , HasPosition platformEnt
  , HasDirection platformEnt
  , HasPlatform platformEnt
  , HasLinearMotion platformEnt
  ) => Seconds -> riderEnt -> platformEnt -> riderEnt
moveOnPlatform delta riderEnt platformEnt
  | entCollision riderEnt platformEnt =
      riderEnt & position +~ (LinearMotion.motionVectorThisStep delta platformEnt)
  | otherwise = riderEnt

-- | TODO: Generalise to AABB instead of CollisionBox + Platform
entCollision ::
  (HasPosition ent1, HasCollisionBox ent1, HasPosition ent2, HasPlatform ent2) => ent1 -> ent2 -> Bool
entCollision ent1 ent2 =
    AABB.collision box1 box2
  where
    box1 = AABB.offset (ent1^.position) (ent1^.collisionBox)
    box2 = AABB.offset (ent2^.position) (ent2^.platform)
