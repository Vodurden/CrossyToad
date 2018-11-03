{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.SDL.Config where

import           Control.Lens
import qualified SDL as SDL

import CrossyToad.Renderer.SDL.Assets

data Config = Config
  { _window :: SDL.Window
  , _renderer :: SDL.Renderer
  , __assets :: Assets
  }

makeClassy ''Config

instance HasAssets Config where
  assets = _assets
