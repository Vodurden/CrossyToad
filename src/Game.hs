module Game where

import           CrossyToad
import           CrossyToad.Runner

import           CrossyToad.Env (Env(..))
import qualified CrossyToad.Scene.Scene as Scene
import           CrossyToad.Effect.Logger.Logger
import qualified CrossyToad.Time.MonadTask.IO.Env as IOMonadTask
import qualified CrossyToad.Effect.Logger.IO.IO as IOLogger
import           CrossyToad.Effect.Logger.LogLevel as LogLevel
import qualified CrossyToad.Effect.Input.SDL.SDL as SDLInput
import qualified CrossyToad.Effect.Renderer.SDL.SDL as SDLRenderer
import qualified CrossyToad.Time.MonadTime.SDL.Env as SDLTime

main :: IO ()
main = do
  sceneEnv <- Scene.initialize
  ioTaskEnv <- IOMonadTask.initialize
  let ioLoggerEnv = IOLogger.initialize LogLevel.all
  sdlInputEnv <- SDLInput.initialize
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
    logText Debug "Starting Crossy Toad!"
    mainLoop
    logText Debug "Bye!"
