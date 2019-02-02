{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.MonadRenderer.SDL.Env where

import           Control.Lens
import           Linear.V2
import           SDL (($=))
import qualified SDL as SDL
import qualified SDL.Font as Font

import           CrossyToad.Renderer.MonadRenderer.SDL.Fonts
import           CrossyToad.Renderer.MonadRenderer.SDL.Textures

data Env = Env
  { _window :: !SDL.Window
  , _renderer :: !SDL.Renderer
  , __textures :: !Textures
  , __fonts :: !Fonts
  }

makeClassy ''Env

instance HasTextures Env where
  textures = _textures

instance HasFonts Env where
  fonts = _fonts

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
  fonts' <- loadFonts
  pure $ Env
    { _window = window'
    , _renderer = renderer'
    , __textures = textures'
    , __fonts = fonts'
    }
