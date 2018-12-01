module CrossyToad.Effect.Renderer.SDL.SDL
  ( module CrossyToad.Effect.Renderer.SDL.Renderer
  , module CrossyToad.Effect.Renderer.SDL.Env
  , module CrossyToad.Effect.Renderer.SDL.Textures
  , initialize
  ) where

import           Linear.V2
import           SDL (($=))
import qualified SDL as SDL
import qualified SDL.Font as Font

import CrossyToad.Effect.Renderer.SDL.Renderer
import CrossyToad.Effect.Renderer.SDL.Env
import CrossyToad.Effect.Renderer.SDL.Textures

initialize :: IO Env
initialize = do
  SDL.initialize [SDL.InitVideo]
  Font.initialize

  window' <- SDL.createWindow "Crossy Toad" SDL.defaultWindow
     { SDL.windowInitialSize = V2 1280 960
     , SDL.windowResizable = True
     }
  renderer' <- SDL.createRenderer window' (-1) SDL.defaultRenderer
  SDL.rendererLogicalSize renderer' $= (Just $ V2 1280 960)
  textures' <- loadTextures renderer'
  pure $ Env
    { _window = window'
    , _renderer = renderer'
    , __textures = textures'
    }
