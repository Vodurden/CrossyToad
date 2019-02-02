module CrossyToad where

import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import           Control.Monad.IO.Class (MonadIO)

import           CrossyToad.Env (Env(..))
import           CrossyToad.Input.MonadInput
import qualified CrossyToad.Input.MonadInput.SDL.MonadInput as SDLMonadInput
import           CrossyToad.Effect.Logger.Logger
import qualified CrossyToad.Effect.Logger.IO.Logger as IOLogger
import           CrossyToad.Effect.Renderer.Renderer
import qualified CrossyToad.Effect.Renderer.SDL.SDL as SDLRenderer
import           CrossyToad.Time.MonadTime
import           CrossyToad.Time.MonadTime.SDL.MonadTime as SDLTime
import           CrossyToad.Time.MonadTask
import qualified CrossyToad.Time.MonadTask.IO.MonadTask as IOMonadTask

newtype CrossyToad a = CrossyToad (ReaderT (Env CrossyToad) IO a)
  deriving (Functor, Applicative, Monad, MonadReader (Env CrossyToad), MonadIO)

runCrossyToad :: Env CrossyToad -> CrossyToad a -> IO a
runCrossyToad config (CrossyToad m) = runReaderT m config

instance MonadInput CrossyToad where
  stepInput = SDLMonadInput.stepInput
  getInputState = SDLMonadInput.getInputState

instance Logger CrossyToad where
  getEnabledLogLevels = IOLogger.getEnabledLogLevels
  logRaw = IOLogger.logRawStdout

instance Renderer CrossyToad where
  runRenderCommand = SDLRenderer.runRenderCommand

instance MonadTime CrossyToad where
  stepTime = SDLTime.stepTime
  deltaTime = SDLTime.deltaTime

instance MonadTask CrossyToad where
  pumpTasks = IOMonadTask.pumpTasks
  forkTask = IOMonadTask.forkTask
