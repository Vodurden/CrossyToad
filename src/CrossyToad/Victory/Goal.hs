{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Victory.Goal where

import           Control.Lens

-- | Represents an entity the player wants to reach to win the game
data Goal = Goal
  { _reached :: !Bool
  } deriving (Eq, Show)

makeClassy ''Goal

mk :: Goal
mk = Goal False
