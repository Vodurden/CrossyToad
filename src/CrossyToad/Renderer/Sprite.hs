{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.Sprite
  ( Sprite(..)
  , HasSprite(..)
  , render
  ) where

import           Control.Lens
import           Control.Monad (mfilter)

import qualified CrossyToad.Renderer.Clip as Clip
import           CrossyToad.Renderer.Asset.ImageAsset (ImageAsset, HasImageAsset(..))
import           CrossyToad.Renderer.RenderCommand (RenderCommand(..))
import           CrossyToad.Geometry.Position
import           CrossyToad.Geometry.Size
import           CrossyToad.Physics.Direction

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
  let screenClip = Clip.mkAt pos (ent ^. sprite . size)
  Draw (ent^.sprite.imageAsset)
       rotation
       Nothing
       (Just screenClip)
