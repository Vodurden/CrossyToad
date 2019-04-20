module CrossyToad.Victory.VictorySystem
  ( jumpScore
  , collectScorable
  ) where

import           Control.Lens

import           CrossyToad.Geometry.Position (HasPosition(..))
import           CrossyToad.Physics.JumpMotion (HasJumpMotion(..))
import qualified CrossyToad.Physics.JumpMotion as JumpMotion
import           CrossyToad.Physics.Physical (HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Victory.Scorable (HasScorable(..))
import           CrossyToad.Victory.Score (HasScore(..))

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
    shouldCollect = Physical.overlapping collectorEnt scorableEnt
                    && scorableEnt ^. scorable.collected == False

    entWithScore = collectorEnt & score.totalScore +~ (scorableEnt^.scorable.value)
    collectedScorable = scorableEnt & scorable . collected .~ True
