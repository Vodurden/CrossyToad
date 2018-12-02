{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Renderer.PixelClip where

import Control.Lens

import CrossyToad.Effect.Renderer.PixelPosition
import CrossyToad.Effect.Renderer.Dimensions

-- | Represents a rectangular "Clip" in pxiels.
-- |
-- | Typically used to specify a rectangular region in an image
-- | or on the screen, such as the source of an image or the target
-- | area to draw to.
data PixelClip = PixelClip
  { _position :: PixelPosition
  , __dimensions :: Dimensions
  } deriving (Eq, Show)

makeClassy ''PixelClip

instance HasDimensions PixelClip where
  dimensions = _dimensions

-- | Represents a PixelClip applied to a texture
type TextureClip = PixelClip

-- | Represnts a PixelClip applied to the screen
type ScreenClip = PixelClip
