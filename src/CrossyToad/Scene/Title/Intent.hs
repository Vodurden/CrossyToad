{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Title.Intent where

import Control.Lens
import Data.Maybe (catMaybes)

import CrossyToad.Engine.InputState
import CrossyToad.Engine.KeyState

data Intent = StartGame
            | Quit
            deriving (Show, Eq)

makeClassyPrisms ''Intent

fromInput :: InputState -> [Intent]
fromInput input = catMaybes $
  [ const StartGame <$> (input ^? enter . _Pressed)
  , const Quit <$> (input ^? esc . _Pressed)
  ]
