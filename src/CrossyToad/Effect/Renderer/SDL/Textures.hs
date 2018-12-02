{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Renderer.SDL.Textures where

import           Control.Lens
import qualified SDL
import qualified SDL.Image as Image

import           CrossyToad.Effect.Renderer.ImageAsset (ImageAsset)
import qualified CrossyToad.Effect.Renderer.ImageAsset as ImageAsset
import           CrossyToad.Effect.Renderer.SDL.Texture (Texture)
import qualified CrossyToad.Effect.Renderer.SDL.Texture as Texture

data Textures = Textures
  { _toad :: !Texture
  , _toad2 :: !Texture
  , _car :: !Texture
  }

makeClassy ''Textures

fromImageAsset :: ImageAsset -> Textures -> Texture
fromImageAsset ImageAsset.Toad = view toad
fromImageAsset ImageAsset.Toad2 = view toad2
fromImageAsset ImageAsset.Car = view car

loadTextures :: SDL.Renderer -> IO Textures
loadTextures renderer = do
    toad' <- loadImage renderer ImageAsset.Toad
    toad2' <- loadImage renderer ImageAsset.Toad2
    car' <- loadImage renderer ImageAsset.Car

    pure $ Textures
      { _toad = toad'
      , _toad2 = toad2'
      , _car = car'
      }

loadImage :: SDL.Renderer -> ImageAsset -> IO Texture
loadImage renderer asset =
  Image.loadTexture renderer (ImageAsset.filepath asset)
    >>= Texture.fromSDL
