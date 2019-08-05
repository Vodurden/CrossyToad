module CrossyToad.Stage.MonadStage
  ( MonadStage(..)
  ) where

import CrossyToad.Stage.Stage (Stage)

class Monad m => MonadStage m where
  stages :: m [Stage]
