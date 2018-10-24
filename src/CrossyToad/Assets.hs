{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Assets where

import           Control.Lens
import           Linear.V4
import qualified SDL
import qualified SDL.Font as Font

data Assets = Assets
  { _titleSprite :: SDL.Texture
  }

makeClassy ''Assets

loadAssets :: SDL.Renderer -> IO Assets
loadAssets renderer = do
    titleFont <- Font.load "assets/font/PrincesS AND THE FROG.ttf" 80
    titleSpriteTexture <- toTexture =<<
      Font.blended titleFont (V4 0xff 0xff 0xff 0xff) " CROSSY TOAD "

    pure $ Assets
      { _titleSprite = titleSpriteTexture
      }
  where
    toTexture surface = SDL.createTextureFromSurface renderer surface
