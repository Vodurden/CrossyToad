{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Input.InputEvent where

import Control.Lens

data Key
  = Return
  | Escape
  | W
  | A
  | S
  | D
  deriving (Show, Eq)

makeClassyPrisms ''Key

data InputEvent
  = KeyPressed Key
  | KeyReleased Key
  deriving (Show, Eq)

makeClassy ''InputEvent
makeClassyPrisms ''InputEvent
