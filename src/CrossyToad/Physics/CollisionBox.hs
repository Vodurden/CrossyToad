{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Physics.CollisionBox
  ( CollisionBox
  , HasCollisionBox(..)
  , module CrossyToad.Geometry.AABB
  , entCollision
  ) where

import Control.Lens

import CrossyToad.Geometry.Geometry
import CrossyToad.Geometry.AABB
import CrossyToad.Geometry.Position (HasPosition(..))

type CollisionBox = AABB 'World

class HasCollisionBox a where
  collisionBox :: Lens' a CollisionBox

instance HasCollisionBox CollisionBox where
  collisionBox = id

-- | Returns true if there is a collision between these two entities
entCollision ::
  ( HasPosition e1 'World
  , HasCollisionBox e1
  , HasPosition e2 'World
  , HasCollisionBox e2
  ) => e1 -> e2 -> Bool
entCollision e1 e2 =
  let
    box1 = offset (e1 ^. position) (e1^.collisionBox)
    box2 = offset (e2 ^. position) (e2^.collisionBox)
  in overlapping box1 box2
