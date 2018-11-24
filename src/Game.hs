module Game where

import           CrossyToad
import           CrossyToad.Runner

import           CrossyToad.Config (Config(..))
import qualified CrossyToad.Renderer.SDL.SDL as SDLRenderer
import           CrossyToad.Vars (initialVars)

main :: IO ()
main = do
  sdlRendererConfig <- SDLRenderer.initialize
  let cfg = Config
            { _sdlRendererConfig = sdlRendererConfig
            }

  runCrossyToad cfg initialVars mainLoop
