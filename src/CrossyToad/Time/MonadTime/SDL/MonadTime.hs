module CrossyToad.Time.MonadTime.SDL.MonadTime
  ( tickTime
  , tickTimeState
  , deltaTime
  ) where

import           Control.Lens
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef
import qualified SDL.Time as Time

import qualified CrossyToad.Time.MonadTime.SDL.MonadTimeState as TimeState
import           CrossyToad.Time.MonadTime.SDL.MonadTimeState (MonadTimeState, HasMonadTimeState(..), HasMonadTimeStateIORef(..))
import           CrossyToad.Time.MonadTime.SDL.Env (HasEnv(..))
import           CrossyToad.Time.Seconds

tickTime ::
  ( MonadReader r m
  , HasEnv r
  , MonadIO m
  ) => m ()
tickTime = do
  timeStateRef' <- view (env.monadTimeStateRef)
  timeNow <- Time.time
  liftIO $ modifyIORef' timeStateRef' (tickTimeState timeNow)

tickTimeState :: Seconds -> MonadTimeState -> MonadTimeState
tickTimeState timeNow ts =
  ts & previousTime .~ (ts ^. currentTime)
     & currentTime .~ timeNow

deltaTime ::
  ( MonadReader r m
  , HasEnv r
  , MonadIO m
  ) => m Seconds
deltaTime = do
  timeStateRef' <- view (env.monadTimeStateRef)
  timeState' <- liftIO $ readIORef timeStateRef'
  pure $ TimeState.deltaTime timeState'
