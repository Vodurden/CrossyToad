{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Sprite.Animation
  ( Animation(..)
  , HasAnimation(..)
  , initialize
  , withAnimation
  , render
  ) where

import           Control.Lens
import           Control.Zipper
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.List.NonEmpty

import           CrossyToad.Effect.Renderer.PixelClip
import           CrossyToad.Effect.Renderer.RenderCommand (RenderCommand(..), AsRenderCommand(..))
import           CrossyToad.Physics.Direction
import           CrossyToad.Physics.Position
import           CrossyToad.Sprite.Sprite (HasSprite(..))
import qualified CrossyToad.Sprite.Sprite as Sprite

data Animation key = Animation
  { _currentKey :: !key
  , _animations :: !(Map key (Top :>> NonEmpty TextureClip :>> TextureClip))
  }

makeClassy ''Animation

initialize :: Animation Int
initialize = Animation
  { _currentKey = 0
  , _animations = Map.empty
  }

withAnimation :: (Ord key) => key -> NonEmpty TextureClip -> Animation key -> Animation key
withAnimation key newClips =
  animation . animations %~ (Map.insert key (zipper newClips & fromWithin traverse))

currentClip :: (Ord key) => Animation key -> TextureClip
currentClip animation' = (animation' ^. animations) ! (animation' ^. currentKey) & view focus

render ::
  ( Ord key
  , HasPosition ent
  , HasDirection ent
  , HasSprite ent
  , HasAnimation ent key
  ) => ent -> RenderCommand
render ent =
  let clip = currentClip (ent ^. animation)
  in (Sprite.render ent) & _Draw . _3 .~ (Just clip)
