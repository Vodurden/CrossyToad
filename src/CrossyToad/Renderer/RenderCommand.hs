{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.RenderCommand where

import Control.Lens
import Data.Degrees (Degrees)
import Data.Text (Text)
import Linear.V2

import CrossyToad.Renderer.Asset.FontAsset (FontAsset)
import CrossyToad.Renderer.Asset.ImageAsset (ImageAsset, AsImageAsset(..))
import CrossyToad.Renderer.Clip
import CrossyToad.Renderer.RGBAColour
import CrossyToad.Geometry.Position

data RenderCommand
  = ClearScreen

  | DrawScreen

  | Draw !ImageAsset
         !(Maybe Clip)
         !(Maybe Clip)
         !(Maybe Degrees)
         !(Maybe (V2 Bool))

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
