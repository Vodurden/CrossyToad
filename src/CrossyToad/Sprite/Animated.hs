{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Sprite.Animated
  ( Animated(..)
  , HasAnimated(..)
  , mk
  , currentAnimation
  , animate
  , render
  ) where

import           Control.Lens
import           Data.Map.Strict (Map)
import           Data.Maybe (fromJust)
import           Control.Zipper.Extended

import           CrossyToad.Effect.Renderer.Clip (HasClip(..))
import           CrossyToad.Effect.Renderer.RenderCommand (RenderCommand(..), AsRenderCommand(..))
import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Direction
import           CrossyToad.Sprite.AnimationFrame (AnimationFrame)
import           CrossyToad.Sprite.Animation (Animation)
import           CrossyToad.Sprite.Sprite (HasSprite(..))
import qualified CrossyToad.Sprite.Sprite as Sprite

-- | The Animated component is used to allow an entity to
-- | be animated.
-- |
-- | It keeps track of the current animation and provide
-- | functions to step and render the correct sprite.
data Animated key = Animated
  { _currentAnimationKey :: key
  , _animations :: Map key Animation
  } deriving (Eq, Show)

makeClassy ''Animated

mk :: key -> Map key [AnimationFrame] -> Animated key
mk initialKey animations' = Animated
  { _currentAnimationKey = initialKey
  , _animations = (fromWithin traverse . zipper) <$> animations'
  }

-- | Switches to the first frame of the given animation
animate :: (Ord key) => key -> Animated key -> Animated key
animate k = (currentAnimationKey .~ k)
            . (animations . (at k) . _Just %~ leftmost)

-- TODO: Can we remove the partiality from this?
currentAnimation :: forall key. (Ord key) => Lens' (Animated key) Animation
currentAnimation = lens getter setter
  where getter :: Animated key -> Animation
        getter animation' = fromJust $ animation' ^. animations . (at $ animation' ^. currentAnimationKey)

        setter :: Animated key -> Animation -> Animated key
        setter animation' animations' =
          animation' & animations . (ix $ animation' ^. currentAnimationKey) .~ animations'

render ::
  ( Ord key
  , HasPosition ent
  , HasDirection ent
  , HasSprite ent
  , HasAnimated ent key
  ) => ent -> RenderCommand
render ent =
  let frame = ent ^. animated . currentAnimation . focus
  in (Sprite.render ent) & _Draw . _3 .~ (Just $ frame ^. clip)
