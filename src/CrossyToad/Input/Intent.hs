{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Input.Intent
  ( Intent(..)
  , AsIntent(..)
  ) where

import Control.Lens

import CrossyToad.Physics.Direction (Direction)

-- | Represents the intent of the user. Depending on the context this could mean
-- | "Move the menu cursor up" or "Move the toad up"
data Intent
  = Move Direction
  | EnterOrConfirm
  | PauseOrExit
  | ForceExit
  deriving (Eq, Show, Ord)

makeClassyPrisms ''Intent
