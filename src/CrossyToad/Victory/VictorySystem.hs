module CrossyToad.Victory.VictorySystem
  ( goalCollision
  , jumpScore
  , collectScorable
  ) where

import           Control.Lens

import           CrossyToad.Geometry.Position (HasPosition(..))
import           CrossyToad.Physics.JumpMotion (HasJumpMotion(..))
import qualified CrossyToad.Physics.JumpMotion as JumpMotion
import           CrossyToad.Physics.Physical (HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Mortality.Mortal (HasMortal(..))
import qualified CrossyToad.Mortality.Mortal as Mortal
import           CrossyToad.Victory.Scorable (HasScorable(..))
import           CrossyToad.Victory.Score (HasScore(..))
import           CrossyToad.Victory.Goal (HasGoal(..))

-- | Respawns the victor and claims the goal if it is not already claimed
goalCollision ::
  ( HasPosition victor
  , HasMortal victor
  , HasPhysical victor

  , HasPosition goalEnt
  , HasPhysical goalEnt
  , HasGoal goalEnt
  ) => victor -> goalEnt -> (victor, goalEnt)
goalCollision victorEnt goalEnt
    | Physical.colliding victorEnt goalEnt && not (goalEnt^.goal.reached) =
      let victorEnt' = Mortal.respawn victorEnt
          goalEnt' = goalEnt & goal.reached .~ True
      in (victorEnt', goalEnt')
    | otherwise = (victorEnt, goalEnt)

-- | Gain score when jumping
jumpScore :: (HasScore ent , HasJumpMotion ent) => ent -> ent
jumpScore ent | JumpMotion.isJumping ent = ent & score . totalScore +~ 1
              | otherwise = ent

collectScorable ::
  ( HasPosition collector
  , HasPhysical collector
  , HasScore collector

  , HasPosition scorable
  , HasPhysical scorable
  , HasScorable scorable)
  => collector -> scorable -> (collector, scorable)
collectScorable collectorEnt scorableEnt
    | shouldCollect = (entWithScore, collectedScorable)
    | otherwise = (collectorEnt, scorableEnt)
  where
    shouldCollect = Physical.colliding collectorEnt scorableEnt
                    && scorableEnt ^. scorable.collected == False

    entWithScore = collectorEnt & score.totalScore +~ (scorableEnt^.scorable.value)
    collectedScorable = scorableEnt & scorable . collected .~ True
