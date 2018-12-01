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
import           CrossyToad.Effect.Time.Time
import qualified CrossyToad.Effect.Time.SDL.Time as SDLTime

newtype CrossyToad a = CrossyToad (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO)

runCrossyToad :: Env -> CrossyToad a -> IO a
runCrossyToad config (CrossyToad m) = runReaderT m config

instance Input CrossyToad where
  stepInput = SDLInput.stepInput
  getInputState = SDLInput.getInputState

instance Logger CrossyToad where
  getEnabledLogLevels = IOLogger.getEnabledLogLevels
  logRaw = IOLogger.logRawStdout

instance Renderer CrossyToad where
  clearScreen = SDLRenderer.clearScreen
  drawScreen = SDLRenderer.drawScreen

  draw = SDLRenderer.draw

instance Time CrossyToad where
  stepTime = SDLTime.stepTime
  deltaTime = SDLTime.deltaTime
