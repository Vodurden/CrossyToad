{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Title.Intent where

import Control.Lens
import Data.Maybe (catMaybes)

import CrossyToad.Effect.Input.Input

data Intent = StartGame
            | Quit
            deriving (Show, Eq)

makeClassyPrisms ''Intent

fromInput :: [InputEvent] -> [Intent]
fromInput events = catMaybes $ fmap fromInput' events
  where
    fromInput' :: InputEvent -> Maybe Intent
    fromInput' (KeyPressed Return) = Just StartGame
    fromInput' (KeyPressed Escape) = Just Quit
    fromInput' _ = Nothing
