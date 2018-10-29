{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Title.Intent where

import Control.Lens
import Data.Maybe (catMaybes)

import CrossyToad.Input.Input

data Intent = StartGame
            | Quit
            deriving (Show, Eq)

makeClassyPrisms ''Intent

fromInput :: InputState -> [Intent]
fromInput input = catMaybes $
  [ const StartGame <$> (input ^? enter . _Pressed)
  , const Quit <$> (input ^? esc . _Pressed)
  ]
