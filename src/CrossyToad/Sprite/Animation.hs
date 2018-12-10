{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Sprite.Animation
  ( Animation(..)
  , HasAnimation(..)
  , render
  ) where

import Control.Lens

import CrossyToad.Effect.Renderer.PixelClip
import CrossyToad.Effect.Renderer.RenderCommand (RenderCommand(..), AsRenderCommand(..))
import CrossyToad.Geometry.Position
import CrossyToad.Physics.Direction
import CrossyToad.Sprite.Sprite (HasSprite(..))
import qualified CrossyToad.Sprite.Sprite as Sprite

data Animation = Animation
  { _textureClip :: !TextureClip
  }

makeClassy ''Animation

render ::
  ( HasPosition ent
  , HasDirection ent
  , HasSprite ent
  , HasAnimation ent
  ) => ent -> RenderCommand
render ent =
  let clip = ent ^. textureClip
  in (Sprite.render ent) & _Draw . _3 .~ (Just clip)
