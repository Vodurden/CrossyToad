module Game where

import           CrossyToad
import           CrossyToad.Runner (mainLoop)

import qualified CrossyToad.Renderer.MonadRenderer.SDL.Env as SDLMonadRenderer
import           CrossyToad.Env (Env(..))
import qualified CrossyToad.Input.MonadInput.SDL.Env as SDLMonadInput
import qualified CrossyToad.Logger.LogLevel as LogLevel
import           CrossyToad.Logger.MonadLogger (logText)
import qualified CrossyToad.Logger.MonadLogger.IO.Env as IOMonadLogger
import qualified CrossyToad.Title.Title as Title
import qualified CrossyToad.Scene.MonadScene.IO.Env as IOMonadScene
import qualified CrossyToad.Time.MonadTask.IO.Env as IOMonadTask
import qualified CrossyToad.Time.MonadTime.SDL.Env as SDLTime

main :: IO ()
main = do
  ioSceneEnv <- IOMonadScene.initialize Title.scene
  ioTaskEnv <- IOMonadTask.initialize
  let ioLoggerEnv = IOMonadLogger.initialize LogLevel.all
  sdlInputEnv <- SDLMonadInput.initialize
  sdlRendererEnv <- SDLMonadRenderer.initialize
  sdlTimeEnv <- SDLTime.initialize
  let cfg = Env
            { _ioSceneEnv = ioSceneEnv
            , _ioTaskEnv = ioTaskEnv
            , _ioLoggerEnv = ioLoggerEnv
            , _sdlInputEnv = sdlInputEnv
            , _sdlRendererEnv = sdlRendererEnv
            , _sdlTimeEnv = sdlTimeEnv
            }

  runCrossyToad cfg $ do
    logText LogLevel.Debug "Starting Crossy Toad!"
    mainLoop
    logText LogLevel.Debug "Bye!"
