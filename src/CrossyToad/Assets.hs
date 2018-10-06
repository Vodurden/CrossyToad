module CrossyToad.Assets where

import           Linear.V4
import qualified SDL
import qualified SDL.Font as Font

data Assets = Assets
  { titleSprite :: SDL.Texture
  }

loadAssets :: SDL.Renderer -> IO Assets
loadAssets renderer = do
    titleFont <- Font.load "assets/font/PrincesS AND THE FROG.ttf" 80
    titleSpriteTexture <- toTexture =<<
      Font.blended titleFont (V4 0xff 0xff 0xff 0xff) " CROSSY TOAD "

    pure $ Assets
      { titleSprite = titleSpriteTexture
      }
  where
    toTexture surface = SDL.createTextureFromSurface renderer surface
