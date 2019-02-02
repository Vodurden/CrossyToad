{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.Clip
  ( Clip
  , HasClip(..)
  , module CrossyToad.Geometry.AABB
  ) where

import Control.Lens

import CrossyToad.Geometry.AABB

-- | Represents a rectangular "Clip" in pxiels.
-- |
-- | Typically used to specify a rectangular region in an image
-- | or on the screen, such as the source of an image or the target
-- | area to draw to.
type Clip = AABB

class HasClip a where
  clip :: Lens' a Clip

instance HasClip Clip where
  clip = id
