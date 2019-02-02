{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Sprite.Animated
  ( Animated(..)
  , HasAnimated(..)
  , mk
  , transition
  , play
  , loop
  , pause
  , currentAnimation
  , step
  , render
  ) where

import           Control.Lens
import           Data.Map.Strict (Map)
import           Data.Maybe (fromJust)

import           CrossyToad.Time.Seconds
import           CrossyToad.Time.MonadTime
import           CrossyToad.Effect.Renderer.Clip (HasClip(..))
import           CrossyToad.Effect.Renderer.RenderCommand (RenderCommand(..), AsRenderCommand(..))
import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Direction
import           CrossyToad.Sprite.AnimationFrame (AnimationFrame)
import           CrossyToad.Sprite.Animation (Animation, currentFrame)
import qualified CrossyToad.Sprite.Animation as Animation
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

step ::
  ( Ord key
  , MonadTime m
  , HasAnimated ent key
  ) => ent -> m ent
step ent = do
  delta <- deltaTime
  pure $ stepBy delta ent

stepBy :: (Ord key, HasAnimated ent key) => Seconds -> ent -> ent
stepBy delta ent =
  ent & animated.currentAnimation %~ (Animation.stepBy delta)

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
