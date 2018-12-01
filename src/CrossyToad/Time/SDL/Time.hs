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
import           CrossyToad.Time.SDL.Env
import           CrossyToad.Time.Seconds

stepTime ::
  ( MonadReader r m
  , HasEnv r
  , MonadIO m
  ) => m ()
stepTime = do
  timeStateRef' <- view (env.timeStateRef)
  timeNow <- Time.time
  liftIO $ modifyIORef' timeStateRef' (stepTimeState timeNow)

stepTimeState :: Seconds -> TimeState -> TimeState
stepTimeState timeNow ts =
  ts & previousTime .~ (ts ^. currentTime)
     & currentTime .~ timeNow

deltaTime ::
  ( MonadReader r m
  , HasEnv r
  , MonadIO m
  ) => m Seconds
deltaTime = do
  timeStateRef' <- view (env.timeStateRef)
  timeState' <- liftIO $ readIORef timeStateRef'
  pure $ TimeState.deltaTime timeState'
