{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Renderer.Dimensions where

import Control.Lens
import Linear.V2

-- | Represents a width and height in pixels
type Dimensions = V2 Int

class HasDimensions a where
  dimensions :: Lens' a Dimensions

instance HasDimensions Dimensions where
  dimensions = id
