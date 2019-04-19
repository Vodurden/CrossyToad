{-# LANGUAGE RankNTypes #-}

-- | This module contains all logic for moving entities around
module CrossyToad.Physics.MovementSystem
  ( moveOnPlatform
  ) where

import           Control.Lens.Extended

import           CrossyToad.Geometry.Position (HasPosition(..))
import           CrossyToad.Physics.Physical (HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Physics.Direction (HasDirection)
import           CrossyToad.Physics.LinearMotion (HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Time.Seconds (Seconds)

-- | Move the rider by the motion of _all_ platforms they are colliding with
-- moveOnAllPlatforms ::
--   ( HasPosition riderEnt
--   , HasPhysical riderEnt

--   , HasPosition platformEnt
--   , HasDirection platformEnt
--   , HasPhysical platformEnt
--   , HasLinearMotion platformEnt
--   ) => Seconds -> Lens' s riderEnt -> Lens' s [platformEnt] -> s -> s
-- moveOnAllPlatforms delta riderL platformsL state =
--   lensFoldl' (moveOnPlatform delta) riderL platformsL state
--   -- state & riderL .~ foldl' (moveOnPlatform delta) (state ^. riderL) (state ^. platformsL)

-- | Move the rider by the motion of the platform if it is
-- | standing on the platform
moveOnPlatform ::
  ( HasPosition riderEnt
  , HasPhysical riderEnt

  , HasPosition platformEnt
  , HasDirection platformEnt
  , HasPhysical platformEnt
  , HasLinearMotion platformEnt
  ) => Seconds -> riderEnt -> platformEnt -> riderEnt
moveOnPlatform delta riderEnt platformEnt
  | Physical.onPlatform riderEnt platformEnt =
      riderEnt & position +~ (LinearMotion.motionVectorThisTick delta platformEnt)
  | otherwise = riderEnt
