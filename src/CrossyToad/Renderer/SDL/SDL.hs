module CrossyToad.Renderer.SDL.SDL
  ( module CrossyToad.Renderer.SDL.Renderer
  , module CrossyToad.Renderer.SDL.Config
  , module CrossyToad.Renderer.SDL.Assets
  , initialize
  ) where

import           Linear.V2
import qualified SDL as SDL
import qualified SDL.Font as Font

import CrossyToad.Renderer.SDL.Renderer
import CrossyToad.Renderer.SDL.Config
import CrossyToad.Renderer.SDL.Assets

initialize :: IO Config
initialize = do
  SDL.initialize [SDL.InitVideo]
  Font.initialize

  window' <- SDL.createWindow "Crossy Toad" SDL.defaultWindow
     { SDL.windowInitialSize = V2 800 640
     }
  renderer' <- SDL.createRenderer window' (-1) SDL.defaultRenderer
  assets' <- loadAssets renderer'
  pure $ Config
    { _window = window'
    , _renderer = renderer'
    , __assets = assets'
    }
