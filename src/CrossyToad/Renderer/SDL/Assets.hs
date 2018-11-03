{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.SDL.Assets where

import           Control.Lens
import           Linear.V4
import qualified SDL
import qualified SDL.Font as Font
import qualified SDL.Image as Image

data Assets = Assets
  { _titleSprite :: SDL.Texture
  , _toad :: SDL.Texture
  }

makeClassy ''Assets

loadAssets :: SDL.Renderer -> IO Assets
loadAssets renderer = do
    titleFont <- Font.load "assets/font/PrincesS AND THE FROG.ttf" 80
    titleSpriteTexture <- toTexture =<<
      Font.blended titleFont (V4 0xff 0xff 0xff 0xff) " CROSSY TOAD "

    toadSprite <- Image.loadTexture renderer "assets/sprite/toad.png"

    pure $ Assets
      { _titleSprite = titleSpriteTexture
      , _toad = toadSprite
      }
  where
    toTexture surface = SDL.createTextureFromSurface renderer surface
