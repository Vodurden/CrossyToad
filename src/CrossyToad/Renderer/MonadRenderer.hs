-- | The @Renderer@ feature is responsible for drawing the screen.
-- |
-- | In particular a renderer knows how to "draw" every visual artifact in
-- | the game such that a human will see it.
module CrossyToad.Renderer.MonadRenderer where

import           Data.Text (Text)
import           Data.Degrees (Degrees)
import           Linear.V2

import           CrossyToad.Geometry.Position
import           CrossyToad.Renderer.Asset.FontAsset (FontAsset)
import           CrossyToad.Renderer.Asset.ImageAsset (ImageAsset)
import           CrossyToad.Renderer.Clip (Clip)
import           CrossyToad.Renderer.RGBAColour (RGBAColour)

class Monad m => MonadRenderer m where
  clearScreen :: m ()
  drawScreen :: m ()
  draw :: ImageAsset -> Maybe Clip -> Maybe Clip -> Maybe Degrees -> Maybe (V2 Bool) -> m ()
  drawAt :: ImageAsset -> Position -> m ()
  drawRect :: Clip -> m ()
  drawText :: FontAsset -> Maybe Degrees -> Maybe Clip -> Maybe Clip -> RGBAColour -> Text -> m ()
