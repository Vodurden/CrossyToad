module CrossyToad.Effect.Renderer.PixelPosition where

import Control.Lens
import Linear.V2

-- | Represents a position in pxiels. This is commonly
-- | used to index images or the screen.
type PixelPosition = (V2 Int)

class HasPixelPosition t where
  pixelPosition :: Lens' t PixelPosition

instance HasPixelPosition PixelPosition where
  pixelPosition = id
