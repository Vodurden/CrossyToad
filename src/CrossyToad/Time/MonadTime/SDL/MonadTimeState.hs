{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Time.MonadTime.SDL.MonadTimeState where


import           Control.Lens
import           Data.IORef
import           Control.Monad.IO.Class (MonadIO)
import qualified SDL.Time as Time

import           CrossyToad.Time.Seconds

data MonadTimeState = MonadTimeState
  { _previousTime :: Seconds
  , _currentTime :: Seconds
  } deriving (Eq, Show)

makeClassy ''MonadTimeState

class HasMonadTimeStateIORef a where
  monadTimeStateRef :: Getter a (IORef MonadTimeState)

instance HasMonadTimeStateIORef (IORef MonadTimeState) where
  monadTimeStateRef = getting id

initialMonadTimeState :: (MonadIO m) => m MonadTimeState
initialMonadTimeState = do
  -- SDL ticks are only valid relative to each other so we need
  -- to make sure we start previous _and_ current time at the same
  -- time otherwise the first "delta" tick will be extremely large
  initialTime <- Time.time
  pure $ MonadTimeState { _previousTime = initialTime
                        , _currentTime = initialTime
                        }

deltaTime :: MonadTimeState -> Seconds
deltaTime ts = (ts^.currentTime) - (ts^.previousTime)
