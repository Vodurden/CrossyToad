{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Input.Key where

import Control.Lens

data Key
  = Return
  | Escape
  | W
  | A
  | S
  | D
  deriving (Show, Eq, Ord)

makeClassyPrisms ''Key
