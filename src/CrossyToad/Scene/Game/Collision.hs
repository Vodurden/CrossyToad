-- | This module is responsible for resolving collisions between
-- | all entities in the game.
module CrossyToad.Scene.Game.Collision
  ( step
  ) where

import           Control.Lens
import           Data.Foldable (foldlM)
import qualified Data.Text as Text

import           CrossyToad.Logger.MonadLogger
import qualified CrossyToad.Logger.LogLevel as LogLevel
import           CrossyToad.Scene.Game.Car (Car, HasCars(..))
import           CrossyToad.Scene.Game.Toad (Toad, HasToad(..))
import qualified CrossyToad.Scene.Game.Toad as Toad
import           CrossyToad.Physics.CollisionBox (HasCollisionBox(..))

step :: (MonadLogger m, HasToad ent, HasCars ent) => ent -> m ent
step ent = do
  nextToad <- foldlM carCollision (ent ^. toad) (ent ^. cars)
  pure $ ent & toad .~ nextToad

carCollision :: (MonadLogger m) => Toad -> Car -> m Toad
carCollision toad' car' | Toad.collision toad' car' = do
                            logText LogLevel.Debug $ (Text.pack "Toad Collision! ") <> (Text.pack $ show $ toad' ^. collisionBox) <> " " <> (Text.pack $ show car')
                            pure $ Toad.die toad'
                        | otherwise = pure toad'
