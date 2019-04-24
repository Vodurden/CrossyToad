{-# LANGUAGE RankNTypes #-}

-- | This module contains all logic for moving entities around
module CrossyToad.Physics.MovementSystem
  ( tickJumping
  , tickLinear
  , moveOnPlatform
  ) where

import           Control.Arrow ((>>>))
import           Control.Lens.Extended
import           Linear.V2

import           CrossyToad.Geometry.Position (HasPosition(..))
import qualified CrossyToad.Geometry.Position as Position
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
    (JumpMotion.tick seconds) >>> airJump >>> enforceMapBoundaries
  where
    airJump :: (HasPhysical ent, HasJumpMotion ent) => ent -> ent
    airJump ent =
      ent & physical . layer .~
        if | JumpMotion.isJumping ent -> Physical.Air
           | otherwise -> Physical.Ground

tickLinear ::
  ( HasPosition ent
  , HasDirection ent
  , HasLinearMotion ent
  ) => Seconds -> ent -> ent
tickLinear seconds =
  (LinearMotion.tick seconds) >>> enforceMapBoundaries

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

-- | If an entity goes out of bounds we need to handle it.
-- |
-- | On the x-axis we loop to the other side of the screen
-- | On the y-axis we cap the movement
enforceMapBoundaries :: (HasPosition ent) => ent -> ent
enforceMapBoundaries = loopXPosition . constrainYPosition
  where
    loopXPosition :: (HasPosition ent) => ent -> ent
    loopXPosition ent
      | ent^.position._x > gridMax^._x = ent & position._x .~ gridMin^._x
      | ent^.position._x < gridMin^._x = ent & position._x .~ gridMax^._x
      | otherwise = ent

    constrainYPosition :: (HasPosition ent) => ent -> ent
    constrainYPosition ent
      | ent ^.position._y > gridMax^._y = ent & position._y .~ gridMax^._y
      | ent ^.position._y < gridMin^._y = ent & position._y .~ gridMin^._y
      | otherwise = ent

    gridMin = (Position.fromGrid (-1) 0)
    gridMax = (Position.fromGrid 20 13)
