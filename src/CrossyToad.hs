module CrossyToad where

import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import           Control.Monad.IO.Class (MonadIO)

import           CrossyToad.Env (Env(..))
import           CrossyToad.Effect.Input.Input
import qualified CrossyToad.Effect.Input.SDL.SDL as SDLInput
import           CrossyToad.Effect.Logger.Logger
import qualified CrossyToad.Effect.Logger.IO.Logger as IOLogger
import           CrossyToad.Effect.Renderer.Renderer
import qualified CrossyToad.Effect.Renderer.SDL.SDL as SDLRenderer
import           CrossyToad.Time.MonadTime
import           CrossyToad.Time.MonadTime.SDL.MonadTime as SDLTime
import           CrossyToad.Effect.Task.MonadTask
import qualified CrossyToad.Effect.Task.IO.MonadTask as IOTask

newtype CrossyToad a = CrossyToad (ReaderT (Env CrossyToad) IO a)
  deriving (Functor, Applicative, Monad, MonadReader (Env CrossyToad), MonadIO)

runCrossyToad :: Env CrossyToad -> CrossyToad a -> IO a
runCrossyToad config (CrossyToad m) = runReaderT m config

instance Input CrossyToad where
  stepInput = SDLInput.stepInput
  getInputState = SDLInput.getInputState

instance Logger CrossyToad where
  getEnabledLogLevels = IOLogger.getEnabledLogLevels
  logRaw = IOLogger.logRawStdout

instance Renderer CrossyToad where
  runRenderCommand = SDLRenderer.runRenderCommand

instance MonadTime CrossyToad where
  stepTime = SDLTime.stepTime
  deltaTime = SDLTime.deltaTime

instance MonadTask CrossyToad where
  pumpTasks = IOTask.pumpTasks
  forkTask = IOTask.forkTask
