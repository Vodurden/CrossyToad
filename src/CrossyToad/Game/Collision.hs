{-# LANGUAGE RankNTypes #-}

-- | This module is responsible for resolving collisions between
-- | all entities in the game.
module CrossyToad.Game.Collision
  ( toadCollision
  ) where

import           Control.Lens
import           Data.Foldable (foldl')

import           CrossyToad.Game.Toad (Toad)
import qualified CrossyToad.Game.Toad as Toad
import           CrossyToad.Geometry.Position (HasPosition(..))
import           CrossyToad.Physics.Physical (HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical

toadCollision :: forall s box. (HasPosition box, HasPhysical box)
              => Lens' s Toad
              -> Lens' s [box]
              -> s
              -> s
toadCollision toadL entsL state =
    state & toadL .~ foldl' toadCollision' (state ^. toadL) (state ^. entsL)
  where
    toadCollision' :: Toad -> box -> Toad
    toadCollision' toad' ent' | Physical.colliding toad' ent' = Toad.die toad'
                              | otherwise = toad'
