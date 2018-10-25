{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Internal where

import Control.Lens

data Scene
  = Title
  | Play
  | Quit
  deriving (Show, Eq)

makeClassy ''Scene
makeClassyPrisms ''Scene
