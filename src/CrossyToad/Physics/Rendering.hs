module CrossyToad.Physics.Rendering
  ( renderPhysical
  ) where

import           Control.Lens

import           CrossyToad.Geometry.AABB (HasAABB(..))
import           CrossyToad.Geometry.Position (HasPosition(..))
import           CrossyToad.Physics.Physical (HasPhysical(..))
import qualified CrossyToad.Renderer.Clip as Clip
import           CrossyToad.Renderer.MonadRenderer (MonadRenderer)
import qualified CrossyToad.Renderer.MonadRenderer as MonadRenderer

renderPhysical ::
  ( MonadRenderer m
  , HasPosition ent
  , HasPhysical ent
  ) => ent -> m ()
renderPhysical ent =
  MonadRenderer.drawRect $ Clip.offset (ent^.position) (ent^.physical.aabb)
