{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | The @Renderer@ feature is responsible for drawing the screen.
-- |
-- | In particular a renderer knows how to "draw" every visual artifact in
-- | the game such that a human will see it.
module CrossyToad.Effect.Renderer.Renderer where

import           Data.Text (Text)
import           Data.Degrees (Degrees)
import           Linear.V2

import           CrossyToad.Effect.Renderer.FontAsset (FontAsset)
import qualified CrossyToad.Effect.Renderer.FontAsset as FontAsset
import           CrossyToad.Effect.Renderer.ImageAsset (ImageAsset)
import qualified CrossyToad.Effect.Renderer.ImageAsset as ImageAsset
import           CrossyToad.Effect.Renderer.PixelClip
import           CrossyToad.Effect.Renderer.PixelPosition
import           CrossyToad.Effect.Renderer.RGBAColour (RGBAColour)
import qualified CrossyToad.Effect.Renderer.RGBAColour as RGBAColour

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen :: m ()

  -- | Draw an image
  draw :: ImageAsset
       -> (Maybe Degrees)
       -> (Maybe TextureClip)
       -> (Maybe ScreenClip)
       -> m ()

  -- | Draw an image at the given screen position
  -- |
  -- | The size of the image will match it's actual resolution
  drawAt :: ImageAsset
         -> PixelPosition
         -> m ()

  -- | Draw some text using the given font.
  -- |
  -- | The size of the font is determined when we load the font
  -- | if we want different sizes we need to load multiple
  -- | versions of the same font.
  drawText :: FontAsset
           -> (Maybe Degrees)
           -> (Maybe TextureClip)
           -> (Maybe ScreenClip)
           -> RGBAColour
           -> Text
           -> m ()

drawToad :: Renderer m => PixelPosition -> m ()
drawToad = drawAt ImageAsset.Toad

drawToad2 :: Renderer m => PixelPosition -> m ()
drawToad2 = drawAt ImageAsset.Toad2

drawCar :: Renderer m => PixelPosition -> m ()
drawCar = drawAt ImageAsset.Car

drawTitleText :: Renderer m => m ()
drawTitleText =
  drawText FontAsset.Title
           Nothing
           Nothing
           Nothing
           RGBAColour.white
           " CROSSY TOAD "
