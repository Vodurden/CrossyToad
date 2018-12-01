module Game where

import           CrossyToad
import           CrossyToad.Runner

import           CrossyToad.Config (Config(..))
import qualified CrossyToad.Renderer.SDL.SDL as SDLRenderer
import qualified CrossyToad.Time.SDL.SDL as SDLTime
import           CrossyToad.Vars (initialVars)

main :: IO ()
main = do
  sdlRendererConfig <- SDLRenderer.initialize
  sdlTimeConfig <- SDLTime.initialize
  let cfg = Config
            { _sdlRendererConfig = sdlRendererConfig
            , _sdlTimeConfig = sdlTimeConfig
            }

  runCrossyToad cfg initialVars mainLoop
