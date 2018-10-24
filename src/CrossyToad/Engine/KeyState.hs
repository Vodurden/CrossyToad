{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Engine.KeyState where

import Control.Lens

data KeyState = Pressed | Held | Released
  deriving (Show, Eq)

makeClassyPrisms ''KeyState
