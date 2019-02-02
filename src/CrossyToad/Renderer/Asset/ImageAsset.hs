{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.Asset.ImageAsset where

import Control.Lens
import System.IO (FilePath)

data ImageAsset
  = Toad
  | Car
  | Grass
  | Water
  | Swamp
  | Road
  deriving (Eq, Show)

makeClassy ''ImageAsset
makeClassyPrisms ''ImageAsset

filepath :: ImageAsset -> FilePath
filepath asset = "assets/sprite/" <> (filename asset)

filename :: ImageAsset -> FilePath
filename Toad = "toad.png"
filename Car = "car.png"
filename Grass = "grass.png"
filename Water = "water.png"
filename Swamp = "swamp.png"
filename Road = "road.png"
