{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Physics.Physical
  ( Physical
  , HasPhysical(..)
  , Layer(..)
  , HasLayer(..)
  , mkAt
  , colliding
  , overlapping
  , onPlatform
  ) where

import           Control.Lens

import           CrossyToad.Geometry.AABB (AABB, HasAABB(..))
import qualified CrossyToad.Geometry.AABB as AABB
import           CrossyToad.Geometry.Position (Position, HasPosition(..))
import           CrossyToad.Geometry.Size (Size)

-- | An entity with this component occupies a physical space in the world.
-- |
-- | In CrossyToad this means it takes up a cube measured in pixels with
-- | an Axis-Aligned Bounding Box provided it's width
data Physical = Physical
  { __aabb :: AABB
  , __layer :: Layer
  } deriving (Eq, Show)

data Layer = Platform | Ground | Air
  deriving (Eq, Show)

makeClassy ''Layer
makeClassyPrisms ''Layer

makeClassy ''Physical

instance HasAABB Physical where aabb = _aabb
instance HasLayer Physical where layer = _layer

mkAt :: Position -> Size -> Layer -> Physical
mkAt pos size layer' = Physical (AABB.mkAt pos size) layer'

-- | Returns true if both entities are sharing the same space (regardless of layer)
overlapping
  :: (HasPosition ent1 , HasPhysical ent1 , HasPosition ent2 , HasPhysical ent2)
  => ent1 -> ent2 -> Bool
overlapping ent1 ent2 =
    AABB.collision box1 box2
  where
    box1 = AABB.offset (ent1^.position) (ent1^.physical.aabb)
    box2 = AABB.offset (ent2^.position) (ent2^.physical.aabb)

-- | Returns true if both entities sharing the same space and on the same layer
colliding
  :: (HasPosition ent1 , HasPhysical ent1 , HasPosition ent2 , HasPhysical ent2)
  => ent1 -> ent2 -> Bool
colliding ent1 ent2 = overlapping ent1 ent2 && ent1 ^. physical.layer == ent2 ^. physical.layer

-- | Returns true if the rider is standing on the platform
onPlatform ::
  ( HasPosition rider
  , HasPhysical rider
  , HasPosition platform
  , HasPhysical platform
  ) => rider -> platform -> Bool
onPlatform rider platform' =
  overlapping rider platform'
  && (rider ^. physical.layer == Ground)
  && (platform' ^. physical.layer == Platform)
