{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Geometry.Internal where

import Linear.V2

data Units = Pixel
           | World
           | Window

-- | An integer that knows which Unit it represents
newtype UnitInt (u :: Units) = UnitInt Int
  deriving (Eq, Show, Num, Enum, Ord)

type Distance (u :: Units) = UnitInt u

type Vec2d (u :: Units) = V2 (UnitInt u)

type Position (u :: Units) = Vec2d u
type Offset (u :: Units) = Vec2d u
type Dimensions (u :: Units) = Vec2d u

-- | The direction an object is facing
data Direction = North | East | South | West
  deriving (Eq, Show, Ord)

-- | How fast an object moves per second
type Speed (u :: Units) = UnitInt u

-- | An axis-aligned bounding box.
-- |
-- | Basically a rectangle that can't be rotated.
data AABB (u :: Units) = AABB
  { _topLeft :: !(Position u)
  , _bottomRight :: !(Position u)
  } deriving (Eq, Show)
