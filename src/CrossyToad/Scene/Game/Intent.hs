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

fromInput :: [InputEvent] -> [Intent]
fromInput events = catMaybes $ fmap fromInput' events
  where
    fromInput' :: InputEvent -> Maybe Intent
    fromInput' (KeyPressed Escape) = Just Exit
    fromInput' (KeyPressed W) = Just $ Move North
    fromInput' (KeyPressed A) = Just $ Move West
    fromInput' (KeyPressed S) = Just $ Move South
    fromInput' (KeyPressed D) = Just $ Move East
    fromInput' _ = Nothing
