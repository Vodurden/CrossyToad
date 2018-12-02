module CrossyToad.Effect.Renderer.FontAsset where

import System.IO (FilePath)

data FontAsset = Title
  deriving (Eq, Show)

filepath :: FontAsset -> FilePath
filepath asset = "assets/font/" <> (filename asset)

filename :: FontAsset -> String
filename Title = "PrincesS AND THE FROG.ttf"
