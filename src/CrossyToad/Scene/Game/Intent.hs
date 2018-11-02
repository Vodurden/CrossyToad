{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.Intent where

import Control.Lens
import Data.Maybe (catMaybes)

import CrossyToad.Input.Input
import CrossyToad.Scene.Game.Direction

data Intent = Move Direction
            | Exit
            deriving (Show, Eq)

makeClassyPrisms ''Intent

fromInput :: InputState -> [Intent]
fromInput input = catMaybes $
  [ const Exit <$> (input ^? esc . _Pressed)
  , const (Move North) <$> (input ^? w . _Pressed)
  , const (Move East) <$> (input ^? a . _Pressed)
  , const (Move South) <$> (input ^? s . _Pressed)
  , const (Move West) <$> (input ^? d . _Pressed)
  ]
