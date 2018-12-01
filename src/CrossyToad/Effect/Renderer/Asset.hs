{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Renderer.Asset where

import Control.Lens

data Asset
  = TitleSprite
  | Toad
  | Car
  deriving (Eq, Show)

makeClassyPrisms ''Asset