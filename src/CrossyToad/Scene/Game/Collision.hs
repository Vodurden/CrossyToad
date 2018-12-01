-- | This module is responsible for resolving collisions between
-- | all entities in the game.
module CrossyToad.Scene.Game.Collision
  ( step
  ) where

import           Control.Lens
import           Data.Foldable (foldlM)
import qualified Data.Text as Text

import           CrossyToad.Effect.Logger.Logger
import           CrossyToad.Scene.Game.Car (Car, HasCars(..))
import           CrossyToad.Scene.Game.Toad (Toad, HasToad(..))
import qualified CrossyToad.Scene.Game.Toad as Toad

step :: (Logger m, HasToad ent, HasCars ent) => ent -> m ent
step ent = do
  nextToad <- foldlM carCollision (ent ^. toad) (ent ^. cars)
  pure $ ent & toad .~ nextToad

carCollision :: (Logger m) => Toad -> Car -> m Toad
carCollision toad' car' | Toad.collision toad' car' = do
                            logText Debug $ (Text.pack "Toad Collision! ") <> (Text.pack $ show toad') <> " " <> (Text.pack $ show car')
                            pure $ Toad.die toad'
                        | otherwise = pure toad'
