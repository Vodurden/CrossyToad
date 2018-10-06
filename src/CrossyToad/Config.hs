module CrossyToad.Config
  ( Config(..)
  ) where

import qualified SDL

import CrossyToad.Assets

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cAssets :: Assets
  }
