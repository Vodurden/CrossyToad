module CrossyToad.Effect.Time.SDL.Time
  ( stepTime
  , stepTimeState
  , deltaTime
  ) where

import           Control.Lens
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef
import qualified SDL.Time as Time

import           CrossyToad.Effect.Time.SDL.TimeState (TimeState, HasTimeState(..), HasTimeStateIORef(..))
import qualified CrossyToad.Effect.Time.SDL.TimeState as TimeState
import           CrossyToad.Effect.Time.SDL.Env
import           CrossyToad.Effect.Time.Seconds

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
