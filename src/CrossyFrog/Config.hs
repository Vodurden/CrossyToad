module CrossyFrog.Config
  ( Config(..)
  ) where

import qualified SDL

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  }

