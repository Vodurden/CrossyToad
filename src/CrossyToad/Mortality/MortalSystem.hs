module CrossyToad.Mortality.MortalSystem where

import           Control.Lens
import           Control.Monad (when)

import           CrossyToad.Geometry.Position (HasPosition(..))
import           CrossyToad.Mortality.Mortal (HasMortal(..))
import qualified CrossyToad.Mortality.Mortal as Mortal
import           CrossyToad.Physics.Physical (HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Scene.MonadScene (MonadScene)
import qualified CrossyToad.Scene.MonadScene as MonadScene
import qualified CrossyToad.Scene.SceneId as SceneId

-- | Kills the victim if it is involved in a collision
mortalCollision ::
  ( HasPosition victim
  , HasMortal victim
  , HasPhysical victim

  , HasPosition assassin
  , HasPhysical assassin
  ) => victim -> assassin -> victim
mortalCollision victim assassin
  | Physical.colliding victim assassin = Mortal.die victim
  | otherwise = victim

checkGameOver :: (MonadScene m, HasMortal player) => player -> m ()
checkGameOver player' = do
  when (player' ^. mortal . lives <= 0) $ do
    MonadScene.delayClear
    MonadScene.delayPush SceneId.Title
