{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Title.Intent where

import Control.Lens
import Data.Maybe (catMaybes)

import           CrossyToad.Input.InputEvent
import qualified CrossyToad.Input.Key as Key

data Intent = StartGame
            | Quit
            deriving (Show, Eq)

makeClassyPrisms ''Intent

fromInput :: [InputEvent] -> [Intent]
fromInput events = catMaybes $ fmap fromInput' events
  where
    fromInput' :: InputEvent -> Maybe Intent
    fromInput' (KeyPressed Key.Return) = Just StartGame
    fromInput' (KeyPressed Key.Escape) = Just Quit
    fromInput' _ = Nothing
