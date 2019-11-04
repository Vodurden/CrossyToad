{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.MonadRenderer.SDL.Fonts where

import           Control.Lens
import           SDL.Font (Font, PointSize)
import qualified SDL.Font as Font

import           CrossyToad.Renderer.Asset.FontAsset (FontAsset)
import qualified CrossyToad.Renderer.Asset.FontAsset as FontAsset

data Fonts = Fonts
  { _titleFont :: !Font
  , _pressStartFont :: !Font
  }

makeClassy ''Fonts

fromFontAsset :: FontAsset -> Fonts -> Font
fromFontAsset FontAsset.Title = view titleFont
fromFontAsset FontAsset.PressStart = view pressStartFont

loadFonts :: IO Fonts
loadFonts = do
    titleFont' <- loadFont FontAsset.Title 80
    pressStartFont' <- loadFont FontAsset.PressStart 80

    pure $ Fonts
      { _titleFont = titleFont'
      , _pressStartFont = pressStartFont'
      }

loadFont :: FontAsset -> PointSize -> IO Font
loadFont asset size = do
  let filepath = FontAsset.filepath asset
  Font.load filepath size
