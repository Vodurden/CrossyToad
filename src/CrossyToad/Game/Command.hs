{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.Command where

import Control.Lens.Extended

import CrossyToad.Geometry.Position
import CrossyToad.Physics.Direction
import CrossyToad.Game.Entity

data Command
  = Spawn Entity Position Direction
  | Kill
  deriving (Eq, Show)

makeClassyPrisms ''Command
