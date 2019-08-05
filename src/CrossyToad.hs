module CrossyToad where

import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import           Control.Monad.IO.Class (MonadIO)

import           CrossyToad.Env (Env(..))
import           CrossyToad.Input.MonadInput
import qualified CrossyToad.Input.MonadInput.SDL.MonadInput as SDLMonadInput
import           CrossyToad.Logger.MonadLogger
import qualified CrossyToad.Logger.MonadLogger.IO.MonadLogger as IOMonadLogger
import           CrossyToad.Renderer.MonadRenderer
import qualified CrossyToad.Renderer.MonadRenderer.SDL.MonadRenderer as SDLMonadRenderer
import           CrossyToad.Scene.MonadScene
import qualified CrossyToad.Scene.MonadScene.IO.MonadScene as IOMonadScene
import           CrossyToad.Stage.MonadStage
import qualified CrossyToad.Stage.MonadStage.IO.MonadStage as IOMonadStage
import           CrossyToad.Time.MonadTask
import qualified CrossyToad.Time.MonadTask.IO.MonadTask as IOMonadTask
import           CrossyToad.Time.MonadTime
import           CrossyToad.Time.MonadTime.SDL.MonadTime as SDLTime

newtype CrossyToad a = CrossyToad (ReaderT (Env CrossyToad) IO a)
  deriving (Functor, Applicative, Monad, MonadReader (Env CrossyToad), MonadIO)

runCrossyToad :: Env CrossyToad -> CrossyToad a -> IO a
runCrossyToad config (CrossyToad m) = runReaderT m config

instance MonadInput CrossyToad where
  tickInput = SDLMonadInput.tickInput

instance MonadLogger CrossyToad where
  getEnabledLogLevels = IOMonadLogger.getEnabledLogLevels
  logRaw = IOMonadLogger.logRawStdout

instance MonadRenderer CrossyToad where
  runRenderCommand = SDLMonadRenderer.runRenderCommand

instance MonadStage CrossyToad where
  stages = IOMonadStage.stages

instance MonadScene CrossyToad where
  handleInputCurrentScene = IOMonadScene.handleInputCurrentScene
  tickCurrentScene = IOMonadScene.tickCurrentScene
  renderCurrentScene = IOMonadScene.renderCurrentScene
  getCurrentScene = IOMonadScene.getCurrentScene
  delayPush = IOMonadScene.delayPush
  delayPop = IOMonadScene.delayPop

instance MonadTime CrossyToad where
  tickTime = SDLTime.tickTime
  deltaTime = SDLTime.deltaTime

instance MonadTask CrossyToad where
  pumpTasks = IOMonadTask.pumpTasks
  forkTask = IOMonadTask.forkTask
