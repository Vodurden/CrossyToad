{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.ScoreZone where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Geometry.Position (Position, HasPosition(..))
import           CrossyToad.Geometry.Size (Size)
import           CrossyToad.Physics.Physical (Physical, HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Victory.Scorable (Scorable, HasScorable(..))
import qualified CrossyToad.Victory.Scorable as Scorable

data ScoreZone = ScoreZone
  { __position :: !Position
  , __physical :: !Physical
  , __scorable :: !Scorable
  } deriving (Eq, Show)

makeClassy ''ScoreZone

instance HasPosition ScoreZone where position = _position
instance HasPhysical ScoreZone where physical = _physical
instance HasScorable ScoreZone where scorable = _scorable

mk :: Int -> Position -> Size -> ScoreZone
mk score pos size' = ScoreZone
  { __position = pos
  , __physical = Physical.mkAt (V2 1 1) size' Physical.Ground
  , __scorable = Scorable.mk score
  }
