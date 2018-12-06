{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Renderer.RenderCommand where

import Control.Lens
import Data.Degrees (Degrees)
import Data.Text (Text)

import CrossyToad.Effect.Renderer.FontAsset (FontAsset)
import CrossyToad.Effect.Renderer.ImageAsset (ImageAsset, AsImageAsset(..))
import CrossyToad.Effect.Renderer.PixelClip
import CrossyToad.Effect.Renderer.PixelPosition
import CrossyToad.Effect.Renderer.RGBAColour

data RenderCommand
  = ClearScreen

  | DrawScreen

  | Draw !ImageAsset
         !(Maybe Degrees)
         !(Maybe TextureClip)
         !(Maybe ScreenClip)

  | DrawAt !ImageAsset
           !PixelPosition

  | DrawText !FontAsset
             !(Maybe Degrees)
             !(Maybe TextureClip)
             !(Maybe ScreenClip)
             !RGBAColour
             !Text
  deriving (Show, Eq)

makeClassy ''RenderCommand
makeClassyPrisms ''RenderCommand

instance AsImageAsset RenderCommand where
  _ImageAsset = _RenderCommand . _ImageAsset
