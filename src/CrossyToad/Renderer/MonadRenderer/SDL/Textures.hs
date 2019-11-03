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
  , _sportsCar :: !Texture
  , _farmTractor :: !Texture
  , _truck :: !Texture
  , _turtle :: !Texture
  , _woodLog :: !Texture
  , _grassSnake :: !Texture
  , _croc :: !Texture
  , _terrain :: !Texture
  }

makeClassy ''Textures

fromImageAsset :: ImageAsset -> Textures -> Texture
fromImageAsset ImageAsset.Toad = view toad
fromImageAsset ImageAsset.ToadHome = view toadHome
fromImageAsset ImageAsset.Car = view car
fromImageAsset ImageAsset.SportsCar = view sportsCar
fromImageAsset ImageAsset.FarmTractor = view farmTractor
fromImageAsset ImageAsset.Truck = view truck
fromImageAsset ImageAsset.Turtle = view turtle
fromImageAsset ImageAsset.WoodLog = view woodLog
fromImageAsset ImageAsset.GrassSnake = view grassSnake
fromImageAsset ImageAsset.Croc = view croc
fromImageAsset ImageAsset.Terrain = view terrain

loadTextures :: SDL.Renderer -> IO Textures
loadTextures renderer = do
    toad' <- loadImage renderer ImageAsset.Toad
    toadHome' <- loadImage renderer ImageAsset.ToadHome
    car' <- loadImage renderer ImageAsset.Car
    sportsCar' <- loadImage renderer ImageAsset.SportsCar
    farmTractor' <- loadImage renderer ImageAsset.FarmTractor
    truck' <- loadImage renderer ImageAsset.Truck
    turtle' <- loadImage renderer ImageAsset.Turtle
    woodLog' <- loadImage renderer ImageAsset.WoodLog
    grassSnake' <- loadImage renderer ImageAsset.GrassSnake
    croc' <- loadImage renderer ImageAsset.Croc
    terrain' <- loadImage renderer ImageAsset.Terrain

    pure $ Textures
      { _toad = toad'
      , _toadHome = toadHome'
      , _car = car'
      , _sportsCar = sportsCar'
      , _farmTractor = farmTractor'
      , _truck = truck'
      , _turtle = turtle'
      , _woodLog = woodLog'
      , _grassSnake = grassSnake'
      , _croc = croc'
      , _terrain = terrain'
      }

loadImage :: SDL.Renderer -> ImageAsset -> IO Texture
loadImage renderer asset =
  Image.loadTexture renderer (ImageAsset.filepath asset)
    >>= Texture.fromSDL
