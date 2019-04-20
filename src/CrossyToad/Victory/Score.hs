{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Victory.Score where

import           Control.Lens

-- | An entity with this component has a score
data Score = Score
  { _totalScore :: !Int
  } deriving (Eq, Show)

makeClassy ''Score

mk :: Score
mk = Score 0
