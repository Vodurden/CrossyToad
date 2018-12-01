{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Config where

import           Control.Lens

import qualified CrossyToad.Renderer.SDL.SDL as SDLRenderer
import qualified CrossyToad.Time.SDL.SDL as SDLTime

data Config = Config
  { _sdlRendererConfig :: SDLRenderer.Config
  , _sdlTimeConfig :: SDLTime.Config
  }

makeClassy ''Config

instance SDLRenderer.HasConfig Config where
  config = sdlRendererConfig

instance SDLTime.HasConfig Config where
  config = sdlTimeConfig
