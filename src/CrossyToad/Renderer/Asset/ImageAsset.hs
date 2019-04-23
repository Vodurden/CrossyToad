{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.Asset.ImageAsset where

import Control.Lens
import System.IO (FilePath)

data ImageAsset
  = Toad
  | ToadHome
  | Car
  | Truck
  | Terrain

  deriving (Eq, Show)

makeClassy ''ImageAsset
makeClassyPrisms ''ImageAsset

filepath :: ImageAsset -> FilePath
filepath asset = "assets/sprite/" <> (filename asset)

filename :: ImageAsset -> FilePath
filename Toad = "toad.png"
filename ToadHome = "toad_home.png"
filename Car = "car.png"
filename Truck = "truck.png"
filename Terrain = "terrain.png"
