{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Renderer.SDL.Env where

import           Control.Lens
import qualified SDL as SDL

import CrossyToad.Effect.Renderer.SDL.Textures

data Env = Env
  { _window :: SDL.Window
  , _renderer :: SDL.Renderer
  , __textures :: Textures
  }

makeClassy ''Env

instance HasTextures Env where
  textures = _textures
