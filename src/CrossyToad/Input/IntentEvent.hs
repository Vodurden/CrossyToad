{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Input.IntentEvent
  ( IntentEvent(..)
  , AsIntentEvent(..)
  ) where

import Control.Lens

import CrossyToad.Input.Intent (Intent)

data IntentEvent
  = Tap Intent
  | Release Intent
  deriving (Eq, Show, Ord)

makeClassyPrisms ''IntentEvent
