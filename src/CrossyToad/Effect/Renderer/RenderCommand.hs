module CrossyToad.Effect.Renderer.RenderCommand where

import Data.Degrees (Degrees)
import Data.Text (Text)

import CrossyToad.Effect.Renderer.FontAsset (FontAsset)
import CrossyToad.Effect.Renderer.ImageAsset (ImageAsset)
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
