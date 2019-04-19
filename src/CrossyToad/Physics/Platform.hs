module CrossyToad.Physics.Platform
  ( Platform
  , HasPlatform(..)
  , module CrossyToad.Geometry.AABB
  ) where

import Control.Lens

import CrossyToad.Geometry.AABB

type Platform = AABB

class HasPlatform a where
  platform :: Lens' a Platform

instance HasPlatform Platform where
  platform = id
