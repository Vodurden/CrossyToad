-- | This module is responsible for resolving collisions between
-- | all entities in the game.
module CrossyToad.Scene.Game.Collision
  ( step
  ) where

import           Control.Lens
import           Control.Monad.State (MonadState)

import           CrossyToad.Scene.Game.Car (Car, HasCars(..))
import           CrossyToad.Scene.Game.Toad (Toad, HasToad(..))
import qualified CrossyToad.Scene.Game.Toad as Toad

step :: (MonadState s m, HasToad s, HasCars s) => m ()
step = do
  toad' <- use $ toad
  cars' <- use $ cars

  toad .= foldl carCollision toad' cars'

carCollision :: Toad -> Car -> Toad
carCollision toad' car' | Toad.collision toad' car' = Toad.die toad'
                        | otherwise = toad'
