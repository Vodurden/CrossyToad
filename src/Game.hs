module Game where

import           CrossyToad
import           CrossyToad.Runner

import           CrossyToad.Env (Env(..))
import qualified CrossyToad.Renderer.SDL.SDL as SDLRenderer
import qualified CrossyToad.Time.SDL.SDL as SDLTime
import           CrossyToad.Vars (initialVars)

main :: IO ()
main = do
  sdlRendererEnv <- SDLRenderer.initialize
  sdlTimeEnv <- SDLTime.initialize
  let cfg = Env
            { _sdlRendererEnv = sdlRendererEnv
            , _sdlTimeEnv = sdlTimeEnv
            }

  runCrossyToad cfg initialVars mainLoop
