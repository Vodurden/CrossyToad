{-# LANGUAGE RankNTypes #-}

-- | This module contains all logic for moving entities around
module CrossyToad.Physics.MovementSystem
  ( tickJumping
  , moveOnPlatform
  ) where

import           Control.Arrow ((>>>))
import           Control.Lens.Extended

import           CrossyToad.Geometry.Position (HasPosition(..))
import           CrossyToad.Physics.Physical (HasPhysical(..), HasLayer(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Physics.Direction (HasDirection)
import           CrossyToad.Physics.LinearMotion (HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Physics.JumpMotion (HasJumpMotion(..))
import qualified CrossyToad.Physics.JumpMotion as JumpMotion
import           CrossyToad.Time.Seconds (Seconds)

tickJumping ::
  ( HasPosition ent
  , HasDirection ent
  , HasPhysical ent
  , HasJumpMotion ent
  ) => Seconds -> ent -> ent
tickJumping seconds =
    (JumpMotion.tick seconds) >>> airJump
  where
    airJump :: (HasPhysical ent, HasJumpMotion ent) => ent -> ent
    airJump ent =
      ent & physical . layer .~
        if | JumpMotion.isJumping ent -> Physical.Air
           | otherwise -> Physical.Ground

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
