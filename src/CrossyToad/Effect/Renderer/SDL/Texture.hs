{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Renderer.SDL.Texture where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified SDL

data Texture = Texture
  { _sdlTexture :: !SDL.Texture
  , _width :: !Int
  , _height :: !Int
  }

makeClassy ''Texture

fromSDL :: MonadIO m => SDL.Texture -> m Texture
fromSDL sdlTexture' = do
  SDL.TextureInfo {textureWidth, textureHeight} <-
    liftIO $ SDL.queryTexture sdlTexture'
  pure $ Texture
    { _sdlTexture = sdlTexture'
    , _width = fromIntegral textureWidth
    , _height = fromIntegral textureHeight
    }
