{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Sprite.Sprite
  ( Sprite(..)
  , HasSprite(..)
  , render
  ) where

import Control.Lens

import CrossyToad.Effect.Renderer.Renderer
import CrossyToad.Effect.Renderer.ImageAsset (ImageAsset, HasImageAsset(..))
import CrossyToad.Effect.Renderer.Dimensions
import CrossyToad.Effect.Renderer.PixelClip (PixelClip(..))
import CrossyToad.Physics.Position

data Sprite = Sprite
  { __imageAsset :: ImageAsset
  , __dimensions :: Dimensions
  } deriving (Eq, Show)

makeClassy ''Sprite

instance HasImageAsset Sprite where
  imageAsset = _imageAsset

instance HasDimensions Sprite where
  dimensions = _dimensions

render :: (Renderer m, HasPosition ent, HasSprite ent) => ent -> m ()
render ent = do
  let pos = truncate <$> ent ^. position
  let screenClip = PixelClip pos (ent ^. sprite . dimensions)
  draw (ent^.sprite.imageAsset)
       Nothing
       Nothing
       (Just screenClip)
