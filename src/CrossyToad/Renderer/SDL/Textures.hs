{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.SDL.Textures where

import           Control.Lens
import           Linear.V4
import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Image as Image

import           CrossyToad.Renderer.Asset (Asset)
import qualified CrossyToad.Renderer.Asset as Asset

data Textures = Textures
  { _titleSprite :: SDL.Texture
  , _toad :: SDL.Texture
  , _car :: SDL.Texture
  }

makeClassy ''Textures

fromAsset :: Asset -> Textures -> SDL.Texture
fromAsset Asset.TitleSprite = view titleSprite
fromAsset Asset.Toad = view toad
fromAsset Asset.Car = view car

loadTextures :: SDL.Renderer -> IO Textures
loadTextures renderer = do
    titleFont <- Font.load "assets/font/PrincesS AND THE FROG.ttf" 80
    titleSpriteTexture <- toTexture =<<
      Font.blended titleFont (V4 0xff 0xff 0xff 0xff) " CROSSY TOAD "

    toadSprite <- Image.loadTexture renderer "assets/sprite/toad.png"
    carSprite <- Image.loadTexture renderer "assets/sprite/car.png"

    pure $ Textures
      { _titleSprite = titleSpriteTexture
      , _toad = toadSprite
      , _car = carSprite
      }
  where
    toTexture surface = SDL.createTextureFromSurface renderer surface
