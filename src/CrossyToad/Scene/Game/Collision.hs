-- | This module is responsible for resolving collisions between
-- | all entities in the game.
module CrossyToad.Scene.Game.Collision
  ( step
  ) where

import           Control.Lens

import           CrossyToad.Scene.Game.Car (Car, HasCars(..))
import           CrossyToad.Scene.Game.Toad (Toad, HasToad(..))
import qualified CrossyToad.Scene.Game.Toad as Toad

step :: (HasToad ent, HasCars ent) => ent -> ent
step ent =
  ent & toad .~ foldl carCollision (ent ^. toad) (ent ^. cars)

carCollision :: Toad -> Car -> Toad
carCollision toad' car' | Toad.collision toad' car' = Toad.die toad'
                        | otherwise = toad'
