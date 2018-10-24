{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Config where

import           Control.Lens
import qualified SDL

import CrossyToad.Assets

data Config = Config
  { _window :: SDL.Window
  , _renderer :: SDL.Renderer
  , __assets :: Assets
  }

makeClassy ''Config

instance HasAssets Config where
  assets = _assets
