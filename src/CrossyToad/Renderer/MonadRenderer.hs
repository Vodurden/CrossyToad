-- | The @Renderer@ feature is responsible for drawing the screen.
-- |
-- | In particular a renderer knows how to "draw" every visual artifact in
-- | the game such that a human will see it.
module CrossyToad.Renderer.MonadRenderer where

import           Control.Lens
import           Data.Text (Text)
import           Data.Degrees (Degrees)
import           Data.Foldable (traverse_)
import           Linear.V2

import           CrossyToad.Geometry.Position
import           CrossyToad.Renderer.RenderCommand (RenderCommand(..))
import           CrossyToad.Renderer.Asset.FontAsset (FontAsset)
import qualified CrossyToad.Renderer.Asset.FontAsset as FontAsset
import           CrossyToad.Renderer.Asset.ImageAsset (ImageAsset)
import           CrossyToad.Renderer.Clip (Clip)
import qualified CrossyToad.Renderer.Clip as Clip
import           CrossyToad.Renderer.RGBAColour (RGBAColour)
import qualified CrossyToad.Renderer.RGBAColour as RGBAColour

class Monad m => MonadRenderer m where
  runRenderCommand :: RenderCommand -> m ()

clearScreen :: (MonadRenderer m) => m ()
clearScreen = runRenderCommand ClearScreen

drawScreen :: (MonadRenderer m) => m ()
drawScreen = runRenderCommand DrawScreen

-- | Draw an image
draw :: (MonadRenderer m)
     => ImageAsset
     -> (Maybe Degrees)
     -> (Maybe Clip)
     -> (Maybe Clip)
     -> m ()
draw a d t s = runRenderCommand (Draw a d t s)

-- | Draw an image at the given screen position
-- |
-- | The size of the image will match it's actual resolution
drawAt :: (MonadRenderer m)
       => ImageAsset
       -> Position
       -> m ()
drawAt a p = runRenderCommand (DrawAt a p)


-- | Draw some text using the given font.
-- |
-- | The size of the font is determined when we load the font
-- | if we want different sizes we need to load multiple
-- | versions of the same font.
drawText :: (MonadRenderer m)
         => FontAsset
         -> (Maybe Degrees)
         -> (Maybe Clip)
         -> (Maybe Clip)
         -> RGBAColour
         -> Text
         -> m ()
drawText a d t s c text = runRenderCommand (DrawText  a d t s c text)

drawTitleText :: MonadRenderer m => m ()
drawTitleText =
  drawText FontAsset.Title
           Nothing
           Nothing
           Nothing
           RGBAColour.white
           " CROSSY TOAD "

-- | Draws a row of tiles
drawTileRow :: (MonadRenderer m)
            => ImageAsset
            -> Position
            -> Int
            -> V2 Double
            -> m ()
drawTileRow asset pos tiles tileDimensions = do
  let tileOffsets = (* tileDimensions^._x) <$> [0..fromIntegral tiles]
  let tilePositions = (\offset -> pos & _x %~ (+offset)) <$> tileOffsets
  (flip traverse_) tilePositions $ \tilePos ->
    draw asset
        Nothing
        Nothing
        (Just $ Clip.mkAt tilePos tileDimensions)
