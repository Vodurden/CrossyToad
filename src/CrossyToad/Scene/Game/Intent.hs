{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.Intent where

import Control.Lens
import Data.Maybe (catMaybes)

import CrossyToad.Input.Input
import CrossyToad.Scene.Game.Direction

data Intent = Move { _direction :: Direction }
            | Exit
            deriving (Show, Eq)

makeClassyPrisms ''Intent

fromInput :: InputState -> [Intent]
fromInput input = catMaybes $
  [ const Exit <$> (input ^? esc ._Pressed)
  ]
