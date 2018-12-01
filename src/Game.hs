module Game where

import           CrossyToad
import           CrossyToad.Runner

import           CrossyToad.Env (Env(..))
import qualified CrossyToad.Scene.Scene as Scene
import qualified CrossyToad.Input.SDL.SDL as SDLInput
import qualified CrossyToad.Renderer.SDL.SDL as SDLRenderer
import qualified CrossyToad.Time.SDL.SDL as SDLTime
import           CrossyToad.Vars (initialVars)

main :: IO ()
main = do
  sceneEnv <- Scene.initialize
  sdlInputEnv <- SDLInput.initialize
  sdlRendererEnv <- SDLRenderer.initialize
  sdlTimeEnv <- SDLTime.initialize
  let cfg = Env
            { _sceneEnv = sceneEnv
            , _sdlInputEnv = sdlInputEnv
            , _sdlRendererEnv = sdlRendererEnv
            , _sdlTimeEnv = sdlTimeEnv
            }

  runCrossyToad cfg initialVars mainLoop
