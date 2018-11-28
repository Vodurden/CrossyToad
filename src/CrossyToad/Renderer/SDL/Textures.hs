{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.SDL.Textures where

import           Control.Lens
import           Linear.V4
import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Image as Image

import           CrossyToad.Renderer.Asset (Asset)
import qualified CrossyToad.Renderer.Asset as Asset
import           CrossyToad.Renderer.SDL.Texture (Texture)
import qualified CrossyToad.Renderer.SDL.Texture as Texture

data Textures = Textures
  { _titleSprite :: Texture
  , _toad :: Texture
  , _car :: Texture
  }

makeClassy ''Textures

fromAsset :: Asset -> Textures -> Texture
fromAsset Asset.TitleSprite = view titleSprite
fromAsset Asset.Toad = view toad
fromAsset Asset.Car = view car

loadTextures :: SDL.Renderer -> IO Textures
loadTextures renderer = do
    let white = (V4 0xff 0xff 0xff 0xff)
    titleFont <- Font.load "assets/font/PrincesS AND THE FROG.ttf" 80
    titleSpriteTexture <- Font.blended titleFont white " CROSSY TOAD "
      >>= toTexture
      >>= Texture.fromSDL

    toadSprite <- Image.loadTexture renderer "assets/sprite/toad.png"
      >>= Texture.fromSDL
    carSprite <- Image.loadTexture renderer "assets/sprite/car.png"
      >>= Texture.fromSDL

    pure $ Textures
      { _titleSprite = titleSpriteTexture
      , _toad = toadSprite
      , _car = carSprite
      }
  where
    toTexture surface = SDL.createTextureFromSurface renderer surface
