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
  , _car :: !Texture
  , _grass :: !Texture
  , _water :: !Texture
  , _swamp :: !Texture
  , _road :: !Texture
  }

makeClassy ''Textures

fromImageAsset :: ImageAsset -> Textures -> Texture
fromImageAsset ImageAsset.Toad = view toad
fromImageAsset ImageAsset.Car = view car
fromImageAsset ImageAsset.Grass = view grass
fromImageAsset ImageAsset.Water = view water
fromImageAsset ImageAsset.Swamp = view swamp
fromImageAsset ImageAsset.Road = view road

loadTextures :: SDL.Renderer -> IO Textures
loadTextures renderer = do
    toad' <- loadImage renderer ImageAsset.Toad
    car' <- loadImage renderer ImageAsset.Car
    grass' <- loadImage renderer ImageAsset.Grass
    water' <- loadImage renderer ImageAsset.Water
    swamp' <- loadImage renderer ImageAsset.Swamp
    road' <- loadImage renderer ImageAsset.Road

    pure $ Textures
      { _toad = toad'
      , _car = car'
      , _grass = grass'
      , _water = water'
      , _swamp = swamp'
      , _road = road'
      }

loadImage :: SDL.Renderer -> ImageAsset -> IO Texture
loadImage renderer asset =
  Image.loadTexture renderer (ImageAsset.filepath asset)
    >>= Texture.fromSDL
