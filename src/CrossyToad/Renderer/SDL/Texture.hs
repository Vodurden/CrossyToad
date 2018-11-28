{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.SDL.Texture where

import           Control.Lens
import Foreign.C.Types

import qualified SDL

data Texture = Texture
  { _sdlTexture :: SDL.Texture
  , _width :: CInt
  , _height :: CInt
  }

makeClassy ''Texture

fromSDL :: SDL.Texture -> IO Texture
fromSDL sdlTexture' = do
  SDL.TextureInfo {textureWidth, textureHeight} <- SDL.queryTexture sdlTexture'
  pure $ Texture
    { _sdlTexture = sdlTexture'
    , _width = textureWidth
    , _height = textureHeight
    }
