{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Renderer.RenderCommand where

import Control.Lens
import Data.Degrees (Degrees)
import Data.Text (Text)

import CrossyToad.Effect.Renderer.FontAsset (FontAsset)
import CrossyToad.Effect.Renderer.ImageAsset (ImageAsset, AsImageAsset(..))
import CrossyToad.Effect.Renderer.Clip
import CrossyToad.Effect.Renderer.RGBAColour
import CrossyToad.Geometry.Position

data RenderCommand
  = ClearScreen

  | DrawScreen

  | Draw !ImageAsset
         !(Maybe Degrees)
         !(Maybe Clip)
         !(Maybe Clip)

  | DrawAt !ImageAsset
           !Position

  | DrawText !FontAsset
             !(Maybe Degrees)
             !(Maybe Clip)
             !(Maybe Clip)
             !RGBAColour
             !Text
  deriving (Show, Eq)

makeClassy ''RenderCommand
makeClassyPrisms ''RenderCommand

instance AsImageAsset RenderCommand where
  _ImageAsset = _RenderCommand . _ImageAsset
