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
import           Linear.V2

import           CrossyToad.Geometry.Position (Position, HasPosition(..))
import           CrossyToad.Geometry.Size (HasSize(..))
import           CrossyToad.Physics.Direction (Direction(..), HasDirection(..))
import           CrossyToad.Renderer.MonadRenderer (MonadRenderer)
import qualified CrossyToad.Renderer.MonadRenderer as MonadRenderer
import           CrossyToad.Renderer.Animation (Animation, currentFrame)
import qualified CrossyToad.Renderer.Animation as Animation
import           CrossyToad.Renderer.Asset.AnimationAsset (AnimationAsset, HasAnimationAsset(frames))
import           CrossyToad.Renderer.Asset.ImageAsset (ImageAsset, HasImageAsset(..))
import           CrossyToad.Renderer.Clip (HasClip(..))
import qualified CrossyToad.Renderer.Clip as Clip
import           CrossyToad.Time.Seconds

-- | An entity with this component can be rendered. This includes
-- | both static rendering and animated rendering
data Animated key = Animated
  { _currentAnimationKey :: !key
  , __imageAsset :: !ImageAsset
  , _animations :: !(Map key Animation)
  } deriving (Eq, Show)

makeClassy ''Animated

instance HasImageAsset (Animated key) where imageAsset = _imageAsset

mk :: key -> AnimationAsset key -> Animated key
mk initialKey animationAsset = Animated
  { _currentAnimationKey = initialKey
  , __imageAsset = animationAsset ^. imageAsset
  , _animations = Animation.mk <$> animationAsset ^. frames
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
  ( MonadRenderer m
  , Ord key
  , HasPosition ent
  , HasDirection ent
  , HasAnimated ent key
  ) => ent -> m ()
render ent = render' (ent^.position) (Just $ ent^.direction) (ent^.animated)

renderNoDirection ::
  ( MonadRenderer m
  , Ord key
  , HasPosition ent
  , HasAnimated ent key
  ) => ent -> m ()
renderNoDirection ent = render' (ent^.position) Nothing (ent^.animated)

render' :: (MonadRenderer m, Ord key) => Position -> Maybe Direction -> Animated key -> m ()
render' pos maybeDir animated' =
  let frame = animated' ^. currentAnimation . currentFrame
      screenClip = Clip.mkAt pos (frame ^. size)
      flipX = maybeDir == (Just East)
      flipY = maybeDir == (Just South)
  in MonadRenderer.draw (animated' ^. imageAsset) (Just $ frame ^. clip) (Just screenClip) Nothing (Just $ V2 flipX flipY)
