module CrossyToad.Time.SDL.Time where

import           Control.Lens
import           Control.Monad.State (MonadState)
import           Control.Monad.IO.Class (MonadIO)
import qualified SDL.Time as Time

import CrossyToad.Time.Seconds
import CrossyToad.Time.SDL.TimeState

stepTime :: (MonadState s m, MonadIO m, HasTimeState s) => m ()
stepTime = do
  currentTimestep' <- use $ timeState.currentTimestep
  timeState.previousTimestep .= currentTimestep'

  currentTime <- Time.time
  timeState.currentTimestep .= currentTime

deltaTime :: (MonadState s m, HasTimeState s) => m Seconds
deltaTime = do
  timeState' <- use timeState
  pure $ (timeState' ^. currentTimestep) - (timeState' ^. previousTimestep)
