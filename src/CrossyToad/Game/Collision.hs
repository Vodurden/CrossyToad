{-# LANGUAGE RankNTypes #-}

-- | This module is responsible for resolving collisions between
-- | all entities in the game.
module CrossyToad.Game.Collision
  ( toadCollision
  ) where

import           Control.Lens
import           Data.Foldable (foldlM)

import           CrossyToad.Logger.MonadLogger
import           CrossyToad.Game.Toad (Toad)
import qualified CrossyToad.Game.Toad as Toad
import           CrossyToad.Geometry.Position (HasPosition(..))
import           CrossyToad.Physics.CollisionBox (HasCollisionBox(..))

toadCollision :: forall s m box. (MonadLogger m, HasPosition box, HasCollisionBox box)
              => Lens' s Toad
              -> Lens' s [box]
              -> s
              -> m s
toadCollision toadL entsL state = do
    nextToad <- foldlM toadCollision' (state ^. toadL) (state ^. entsL)
    pure $ state & toadL .~ nextToad
  where
    toadCollision' :: Toad -> box -> m Toad
    toadCollision' toad' ent' | Toad.collision toad' ent' = do
                                  pure $ Toad.die toad'
                              | otherwise = pure toad'

-- step :: (MonadLogger m, HasToad ent, HasCar ent) => ent -> m ent
-- step ent = do
--   nextToad <- foldlM carCollision (ent ^. toad) (ent ^. cars)
--   pure $ ent & toad .~ nextToad

-- carCollision :: (MonadLogger m) => Toad -> Car -> m Toad
-- carCollision toad' car' | Toad.collision toad' car' = do
--                             logText LogLevel.Debug $ (Text.pack "Toad Collision! ") <> (Text.pack $ show $ toad' ^. collisionBox) <> " " <> (Text.pack $ show car')
--                             pure $ Toad.die toad'
--                         | otherwise = pure toad'
