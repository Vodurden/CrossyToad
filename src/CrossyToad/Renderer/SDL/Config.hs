{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.SDL.Config where

import           Control.Lens
import qualified SDL as SDL

import CrossyToad.Renderer.SDL.Textures

data Config = Config
  { _window :: SDL.Window
  , _renderer :: SDL.Renderer
  , __textures :: Textures
  }

makeClassy ''Config

instance HasTextures Config where
  textures = _textures
