{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Input.InputEvent where

import Control.Lens

import CrossyToad.Input.Key

data InputEvent
  = KeyPressed Key
  | KeyReleased Key
  deriving (Show, Eq)

makeClassy ''InputEvent
makeClassyPrisms ''InputEvent
