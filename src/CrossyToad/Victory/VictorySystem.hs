module CrossyToad.Victory.VictorySystem
  ( collectScorable
  ) where

import           Control.Lens

import           CrossyToad.Geometry.Position (HasPosition(..))
import           CrossyToad.Physics.Physical (HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Victory.Score (HasScore(..))
import           CrossyToad.Victory.Scorable (HasScorable(..))

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
