{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.Command where

import Control.Lens.Extended

import CrossyToad.Geometry.Position (Position)
import CrossyToad.Physics.Direction (Direction)
import CrossyToad.Physics.Speed (Speed)
import CrossyToad.Game.Entity (Entity(..))

data Command
  = Spawn Entity Position Direction Speed
  | Kill
  deriving (Eq, Show)

makeClassyPrisms ''Command
