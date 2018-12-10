-- | The @Renderer@ feature is responsible for drawing the screen.
-- |
-- | In particular a renderer knows how to "draw" every visual artifact in
-- | the game such that a human will see it.
module CrossyToad.Effect.Renderer.Renderer where

import           Control.Lens
import           Data.Text (Text)
import           Data.Degrees (Degrees)
import           Linear.V2

import           CrossyToad.Geometry.Position
import           CrossyToad.Effect.Renderer.RenderCommand (RenderCommand(..))
import           CrossyToad.Effect.Renderer.FontAsset (FontAsset)
import qualified CrossyToad.Effect.Renderer.FontAsset as FontAsset
import           CrossyToad.Effect.Renderer.ImageAsset (ImageAsset)
import           CrossyToad.Effect.Renderer.Clip (Clip)
import qualified CrossyToad.Effect.Renderer.Clip as Clip
import           CrossyToad.Effect.Renderer.RGBAColour (RGBAColour)
import qualified CrossyToad.Effect.Renderer.RGBAColour as RGBAColour

class Monad m => Renderer m where
  runRenderCommand :: RenderCommand -> m ()

clearScreen :: (Renderer m) => m ()
clearScreen = runRenderCommand ClearScreen

drawScreen :: (Renderer m) => m ()
drawScreen = runRenderCommand DrawScreen

-- | Draw an image
draw :: (Renderer m)
     => ImageAsset
     -> (Maybe Degrees)
     -> (Maybe Clip)
     -> (Maybe Clip)
     -> m ()
draw a d t s = runRenderCommand (Draw a d t s)

-- | Draw an image at the given screen position
-- |
-- | The size of the image will match it's actual resolution
drawAt :: (Renderer m)
       => ImageAsset
       -> Position
       -> m ()
drawAt a p = runRenderCommand (DrawAt a p)


-- | Draw some text using the given font.
-- |
-- | The size of the font is determined when we load the font
-- | if we want different sizes we need to load multiple
-- | versions of the same font.
drawText :: (Renderer m)
         => FontAsset
         -> (Maybe Degrees)
         -> (Maybe Clip)
         -> (Maybe Clip)
         -> RGBAColour
         -> Text
         -> m ()
drawText a d t s c text= runRenderCommand (DrawText  a d t s c text)

drawTitleText :: Renderer m => m ()
drawTitleText =
  drawText FontAsset.Title
           Nothing
           Nothing
           Nothing
           RGBAColour.white
           " CROSSY TOAD "

-- | Draws a row of tiles
drawTileRow :: ImageAsset
            -> Position
            -> Int
            -> V2 Float
            -> [RenderCommand]
drawTileRow asset pos tiles tileDimensions = do
  let tileOffsets = (* tileDimensions^._x) <$> [0..fromIntegral tiles]
  let tilePositions = (\offset -> pos & _x %~ (+offset)) <$> tileOffsets
  (flip fmap) tilePositions $ \tilePos ->
    Draw asset
        Nothing
        Nothing
        (Just $ Clip.mkAt tilePos tileDimensions)
