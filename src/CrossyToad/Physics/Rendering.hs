module CrossyToad.Physics.Rendering
  ( renderPhysical
  ) where

import           Control.Lens

import           CrossyToad.Geometry.AABB (HasAABB(..))
import           CrossyToad.Geometry.Position (HasPosition(..))
import           CrossyToad.Physics.Physical (HasPhysical(..))
import qualified CrossyToad.Renderer.Clip as Clip
import           CrossyToad.Renderer.RenderCommand (RenderCommand(..))

renderPhysical ::
  ( HasPosition ent
  , HasPhysical ent
  ) => ent -> RenderCommand
renderPhysical ent =
  DrawRect $ Clip.offset (ent^.position) (ent^.physical.aabb)
