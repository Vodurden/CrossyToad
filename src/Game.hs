module Game where

import           CrossyToad
import           CrossyToad.Runner

import qualified CrossyToad.Effect.Renderer.SDL.SDL as SDLRenderer
import           CrossyToad.Env (Env(..))
import qualified CrossyToad.Input.MonadInput.SDL.Env as SDLMonadInput
import qualified CrossyToad.Logger.LogLevel as LogLevel
import           CrossyToad.Logger.MonadLogger (logText)
import qualified CrossyToad.Logger.MonadLogger.IO.Env as IOMonadLogger
import qualified CrossyToad.Scene.Scene as Scene
import qualified CrossyToad.Time.MonadTask.IO.Env as IOMonadTask
import qualified CrossyToad.Time.MonadTime.SDL.Env as SDLTime

main :: IO ()
main = do
  sceneEnv <- Scene.initialize
  ioTaskEnv <- IOMonadTask.initialize
  let ioLoggerEnv = IOMonadLogger.initialize LogLevel.all
  sdlInputEnv <- SDLMonadInput.initialize
  sdlRendererEnv <- SDLRenderer.initialize
  sdlTimeEnv <- SDLTime.initialize
  let cfg = Env
            { _sceneEnv = sceneEnv
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
