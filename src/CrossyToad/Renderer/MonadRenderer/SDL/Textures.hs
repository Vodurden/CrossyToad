{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.MonadRenderer.SDL.Textures where

import           Control.Lens
import qualified SDL
import qualified SDL.Image as Image

import           CrossyToad.Renderer.Asset.ImageAsset (ImageAsset)
import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset
import           CrossyToad.Renderer.MonadRenderer.SDL.Texture (Texture)
import qualified CrossyToad.Renderer.MonadRenderer.SDL.Texture as Texture

data Textures = Textures
  { _toad :: !Texture
  , _toadHome :: !Texture
  , _car :: !Texture
  , _truck :: !Texture
  , _terrain :: !Texture
  }

makeClassy ''Textures

fromImageAsset :: ImageAsset -> Textures -> Texture
fromImageAsset ImageAsset.Toad = view toad
fromImageAsset ImageAsset.ToadHome = view toadHome
fromImageAsset ImageAsset.Car = view car
fromImageAsset ImageAsset.Truck = view truck
fromImageAsset ImageAsset.Terrain = view terrain

loadTextures :: SDL.Renderer -> IO Textures
loadTextures renderer = do
    toad' <- loadImage renderer ImageAsset.Toad
    toadHome' <- loadImage renderer ImageAsset.ToadHome
    car' <- loadImage renderer ImageAsset.Car
    truck' <- loadImage renderer ImageAsset.Truck
    terrain' <- loadImage renderer ImageAsset.Terrain

    pure $ Textures
      { _toad = toad'
      , _toadHome = toadHome'
      , _car = car'
      , _truck = truck'
      , _terrain = terrain'
      }

loadImage :: SDL.Renderer -> ImageAsset -> IO Texture
loadImage renderer asset =
  Image.loadTexture renderer (ImageAsset.filepath asset)
    >>= Texture.fromSDL
