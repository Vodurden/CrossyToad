module CrossyToad.Time.SDL.Time
  ( stepTime
  , stepTimeState
  , deltaTime
  ) where

import           Control.Lens
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef
import qualified SDL.Time as Time

import           CrossyToad.Time.SDL.TimeState (TimeState, HasTimeState(..), HasTimeStateIORef(..))
import qualified CrossyToad.Time.SDL.TimeState as TimeState
import           CrossyToad.Time.SDL.Config
import           CrossyToad.Time.Seconds

stepTime ::
  ( MonadReader env m
  , MonadIO m
  , HasConfig env
  ) => m ()
stepTime = do
  timeStateRef' <- view (config.timeStateRef)
  timeNow <- Time.time
  liftIO $ modifyIORef' timeStateRef' (stepTimeState timeNow)

stepTimeState :: Seconds -> TimeState -> TimeState
stepTimeState timeNow ts =
  ts & previousTime .~ (ts ^. currentTime)
     & currentTime .~ timeNow

deltaTime ::
  ( MonadReader env m
  , MonadIO m
  , HasConfig env
  ) => m Seconds
deltaTime = do
  timeStateRef' <- view (config.timeStateRef)
  timeState' <- liftIO $ readIORef timeStateRef'
  pure $ TimeState.deltaTime timeState'
