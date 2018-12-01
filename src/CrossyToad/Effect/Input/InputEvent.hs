{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Input.InputEvent where

import Control.Lens

import CrossyToad.Effect.Input.Key

data InputEvent
  = KeyPressed Key
  | KeyReleased Key
  | QuitEvent
  deriving (Show, Eq)

makeClassy ''InputEvent
makeClassyPrisms ''InputEvent
