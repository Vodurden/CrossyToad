{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module CrossyToad.Effect.Renderer.PixelClip where

import Control.Lens
import Linear.V2

import CrossyToad.Effect.Renderer.PixelPosition

-- | Represents a rectangular "Clip" in pxiels.
-- |
-- | Typically used to specify a rectangular region in an image
-- | or on the screen, such as the source of an image or the target
-- | area to draw to.
data PixelClip = PixelClip
  { _topLeft :: PixelPosition
  , _bottomRight :: PixelPosition
  }

-- | Represents a PixelClip applied to a texture
type TextureClip = PixelClip

-- | Represnts a PixelClip applied to the screen
type ScreenClip = PixelClip

makeClassy ''PixelClip

dimensions :: Getter PixelClip (V2 Int)
dimensions = to dimensions'
  where dimensions' :: PixelClip -> V2 Int
        dimensions' clip =
          let w = clip^.bottomRight._x - clip^.topLeft._x
              h = clip^.bottomRight._y - clip^.topLeft._y
          in V2 w h

width :: Getter PixelClip Int
width = dimensions . _x

height :: Getter PixelClip Int
height = dimensions . _y
