{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Renderer.Asset where

import Control.Lens

data Asset
  = TitleSprite
  | Toad
  | Toad2
  | Car
  deriving (Eq, Show)

makeClassyPrisms ''Asset