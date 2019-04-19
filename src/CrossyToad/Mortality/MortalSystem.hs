module CrossyToad.Mortality.MortalSystem where

import           CrossyToad.Geometry.Position (HasPosition(..))
import           CrossyToad.Mortality.Mortal (HasMortal(..))
import qualified CrossyToad.Mortality.Mortal as Mortal
import           CrossyToad.Physics.Physical (HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical

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
