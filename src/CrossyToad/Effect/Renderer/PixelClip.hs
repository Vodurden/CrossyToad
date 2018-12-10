{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Renderer.PixelClip where

import Control.Lens

import CrossyToad.Geometry.Position
import CrossyToad.Geometry.Size

-- | Represents a rectangular "Clip" in pxiels.
-- |
-- | Typically used to specify a rectangular region in an image
-- | or on the screen, such as the source of an image or the target
-- | area to draw to.
data PixelClip = PixelClip
  { __position :: Position
  , __size :: Size
  } deriving (Eq, Show)

makeClassy ''PixelClip

instance HasPosition PixelClip where
  position = _position

instance HasSize PixelClip where
  size = _size

-- | Represents a PixelClip applied to a texture
type TextureClip = PixelClip

-- | Represnts a PixelClip applied to the screen
type ScreenClip = PixelClip
