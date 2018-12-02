{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Renderer.ImageAsset where

import Control.Lens
import System.IO (FilePath)

data ImageAsset
  = Toad
  | Toad2
  | Car
  deriving (Eq, Show)

makeClassy ''ImageAsset
makeClassyPrisms ''ImageAsset

filepath :: ImageAsset -> FilePath
filepath asset = "assets/sprite/" <> (filename asset)

filename :: ImageAsset -> FilePath
filename Toad = "toad.png"
filename Toad2 = "toad2.png"
filename Car = "car.png"
