{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.RenderCommand where

import Control.Lens
import Data.Degrees (Degrees)
import Data.Text (Text)

import CrossyToad.Renderer.FontAsset (FontAsset)
import CrossyToad.Renderer.ImageAsset (ImageAsset, AsImageAsset(..))
import CrossyToad.Renderer.Clip
import CrossyToad.Renderer.RGBAColour
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
