{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Input.Key where

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

