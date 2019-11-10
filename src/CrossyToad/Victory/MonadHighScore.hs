module CrossyToad.Victory.MonadHighScore
  ( MonadHighScore(..)
  ) where

import CrossyToad.Victory.HighScore (HighScore)

class Monad m => MonadHighScore m where
  highScores :: m [HighScore]
  saveScore :: HighScore -> m ()
