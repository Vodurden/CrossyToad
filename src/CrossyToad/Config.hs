{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Config where

import           Control.Lens

import qualified CrossyToad.Renderer.SDL.SDL as SDLRenderer

data Config = Config
  { _sdlRendererConfig :: SDLRenderer.Config
  }

makeClassy ''Config

instance SDLRenderer.HasConfig Config where
  config = sdlRendererConfig
