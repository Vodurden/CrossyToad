{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.SDL.Env where

import           Control.Lens
import qualified SDL as SDL

import CrossyToad.Renderer.SDL.Textures

data Env = Env
  { _window :: SDL.Window
  , _renderer :: SDL.Renderer
  , __textures :: Textures
  }

makeClassy ''Env

instance HasTextures Env where
  textures = _textures
