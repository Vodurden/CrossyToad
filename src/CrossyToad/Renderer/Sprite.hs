{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.Sprite
  ( Sprite(..)
  , HasSprite(..)
  , render
  , renderNoDirection
  ) where

import           Control.Lens
import           Control.Monad (mfilter)

import qualified CrossyToad.Renderer.Clip as Clip
import           CrossyToad.Renderer.Asset.ImageAsset (ImageAsset, HasImageAsset(..))
import           CrossyToad.Renderer.RenderCommand (RenderCommand(..))
import           CrossyToad.Geometry.Position
import           CrossyToad.Geometry.Size
import           CrossyToad.Physics.Direction (Direction, HasDirection(..))
import qualified CrossyToad.Physics.Direction as Direction

data Sprite = Sprite
  { __imageAsset :: !ImageAsset
  , __size :: !Size
  } deriving (Eq, Show)

makeClassy ''Sprite

instance HasImageAsset Sprite where
  imageAsset = _imageAsset

instance HasSize Sprite where
  size = _size

render :: (HasPosition ent, HasDirection ent, HasSprite ent) => ent -> RenderCommand
render ent = render' (ent^.position) (Just $ ent^.direction) (ent^.sprite)

renderNoDirection :: (HasPosition ent, HasSprite ent) => ent -> RenderCommand
renderNoDirection ent = render' (ent^.position) Nothing (ent^.sprite)

render' :: Position -> Maybe Direction -> Sprite -> RenderCommand
render' pos maybeDir sprite' =
  let rotation = mfilter (/= 0) (Direction.degrees <$> maybeDir)
      screenClip = Clip.mkAt pos (sprite' ^. size)
  in Draw (sprite' ^. imageAsset)
          rotation
          Nothing
          (Just screenClip)
