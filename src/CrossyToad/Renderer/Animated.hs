{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Renderer.Animated
  ( Animated(..)
  , HasAnimated(..)
  , mk
  , transition
  , play
  , loop
  , pause
  , currentAnimation
  , tick
  , render
  , renderNoDirection
  ) where

import           Control.Lens
import           Data.Map.Strict (Map)
import           Data.Maybe (fromJust)

import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Direction
import           CrossyToad.Renderer.Animation (Animation, currentFrame)
import qualified CrossyToad.Renderer.Animation as Animation
import           CrossyToad.Renderer.AnimationFrame (AnimationFrame)
import           CrossyToad.Renderer.Clip (HasClip(..))
import           CrossyToad.Renderer.RenderCommand (RenderCommand(..), AsRenderCommand(..))
import           CrossyToad.Renderer.Sprite (HasSprite(..))
import qualified CrossyToad.Renderer.Sprite as Sprite
import           CrossyToad.Time.Seconds

-- | The Animated component is used to allow an entity to
-- | be animated.
-- |
-- | It keeps track of the current animation and provide
-- | functions to tick and render the correct sprite.
data Animated key = Animated
  { _currentAnimationKey :: key
  , _animations :: Map key Animation
  } deriving (Eq, Show)

makeClassy ''Animated

mk :: key -> Map key [AnimationFrame] -> Animated key
mk initialKey animations' = Animated
  { _currentAnimationKey = initialKey
  , _animations = Animation.mk <$> animations'
  }

-- | Transition to a new animated state if we are not already
-- | executing that animation
transition
  :: (Ord key, HasAnimated ent key)
  => (key -> ent -> ent)
  -> key
  -> ent
  -> ent
transition f key anim
  | anim^.currentAnimationKey /= key = f key anim
  | otherwise = anim


-- | Switches to the given animation and plays it once.
play :: (Ord key , HasAnimated ent key)
     => key
     -> ent
     -> ent
play k = (currentAnimationKey .~ k)
         . (animations . (at k) . _Just %~ Animation.play)

-- | Switches to the given animation and loops it.
loop :: (Ord key) => key -> Animated key -> Animated key
loop k = (currentAnimationKey .~ k)
         . (animations . (at k) . _Just %~ Animation.loop)

pause :: (Ord key) => key -> Animated key -> Animated key
pause k = (currentAnimationKey .~ k)
          . (animations . (at k) . _Just %~ Animation.pause)

-- TODO: Can we remove the partiality from this?
currentAnimation :: forall key. (Ord key) => Lens' (Animated key) Animation
currentAnimation = lens getter setter
  where getter :: Animated key -> Animation
        getter animation' = fromJust $ animation' ^. animations . (at $ animation' ^. currentAnimationKey)

        setter :: Animated key -> Animation -> Animated key
        setter animation' animations' =
          animation' & animations . (ix $ animation' ^. currentAnimationKey) .~ animations'

tick :: (Ord key, HasAnimated ent key) => Seconds -> ent -> ent
tick delta ent =
  ent & animated.currentAnimation %~ (Animation.tick delta)

render ::
  ( Ord key
  , HasPosition ent
  , HasDirection ent
  , HasSprite ent
  , HasAnimated ent key
  ) => ent -> RenderCommand
render ent =
  let frame = ent ^. animated . currentAnimation . currentFrame
  in (Sprite.render ent) & _Draw . _3 .~ (Just $ frame ^. clip)

renderNoDirection ::
  ( Ord key
  , HasPosition ent
  , HasSprite ent
  , HasAnimated ent key
  ) => ent -> RenderCommand
renderNoDirection ent =
  let frame = ent ^. animated . currentAnimation . currentFrame
  in (Sprite.renderNoDirection ent) & _Draw . _3 .~ (Just $ frame ^. clip)
