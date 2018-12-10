{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Sprite.Sprite
  ( Sprite(..)
  , HasSprite(..)
  , render
  ) where

import Control.Lens
import Control.Monad (mfilter)

import CrossyToad.Effect.Renderer.ImageAsset (ImageAsset, HasImageAsset(..))
import CrossyToad.Effect.Renderer.PixelClip (PixelClip(..))
import CrossyToad.Effect.Renderer.RenderCommand (RenderCommand(..))
import CrossyToad.Geometry.Position
import CrossyToad.Geometry.Size
import CrossyToad.Physics.Direction

data Sprite = Sprite
  { __imageAsset :: !ImageAsset
  , __size :: !Size
  } deriving (Eq, Show)

makeClassy ''Sprite

instance HasImageAsset Sprite where
  imageAsset = _imageAsset

instance HasSize Sprite where
  size = _size

render ::
  ( HasPosition ent
  , HasDirection ent
  , HasSprite ent
  ) => ent -> RenderCommand
render ent = do
  let pos = ent ^. position
  let rotation = mfilter (/= 0) (Just $ degrees (ent^.direction))
  let screenClip = PixelClip pos (ent ^. sprite . size)
  Draw (ent^.sprite.imageAsset)
       rotation
       Nothing
       (Just screenClip)
