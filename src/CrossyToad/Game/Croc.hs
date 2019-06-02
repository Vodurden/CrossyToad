{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module CrossyToad.Game.Croc
  ( Croc(..)
  , HasCroc(..)
  , CrocHead(..)
  , CrocBody(..)
  , mk
  ) where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Geometry.Position (Position, HasPosition(..))
import           CrossyToad.Physics.Direction (Direction(..), HasDirection(..))
import           CrossyToad.Physics.LinearMotion (LinearMotion, HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Physics.Physical (Physical, HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Physics.Speed (Speed)
import           CrossyToad.Physics.Submersible (Submersible, HasSubmersible(..))
import qualified CrossyToad.Physics.Submersible as Submersible
import           CrossyToad.Renderer.Animated (Animated(..), HasAnimated(..))
import qualified CrossyToad.Renderer.Animated as Animated
import           CrossyToad.Renderer.Asset.Animation.Croc as CrocAnimation

data Croc = Croc
  { __position :: !Position
  , __direction :: !Direction
  , __linearMotion :: !LinearMotion
  , __headPhysical :: !Physical
  , __bodyPhysical :: !Physical
  , __bodySubmersible :: !Submersible
  , __animated :: !(Animated CrocAnimation.Animation)
  } deriving (Eq, Show)

makeClassy ''Croc

instance HasPosition Croc where position = _position
instance HasDirection Croc where direction = _direction
instance HasLinearMotion Croc where linearMotion = _linearMotion
instance HasAnimated Croc CrocAnimation.Animation where animated = _animated

newtype CrocHead = CrocHead { unCrocHead :: Croc }

instance Wrapped CrocHead where
  type Unwrapped CrocHead = Croc
  _Wrapped' = iso unCrocHead CrocHead

instance HasPosition CrocHead where position = _Wrapped' . position
instance HasDirection CrocHead where direction = _Wrapped' . _direction
instance HasLinearMotion CrocHead where linearMotion = _Wrapped' . _linearMotion
instance HasAnimated CrocHead CrocAnimation.Animation where animated = _Wrapped' . _animated
instance HasPhysical CrocHead where physical = _Wrapped' . _headPhysical
instance HasSubmersible CrocHead where submersible = _Wrapped' . _bodySubmersible

newtype CrocBody = CrocBody { unCrocBody :: Croc }

instance Wrapped CrocBody where
  type Unwrapped CrocBody = Croc
  _Wrapped' = iso unCrocBody CrocBody

instance HasPosition CrocBody where position = _Wrapped' . position
instance HasDirection CrocBody where direction = _Wrapped' . _direction
instance HasLinearMotion CrocBody where linearMotion = _Wrapped' . _linearMotion
instance HasAnimated CrocBody CrocAnimation.Animation where animated = _Wrapped' . _animated
instance HasPhysical CrocBody where physical = _Wrapped' . _bodyPhysical

mk :: Position -> Direction -> Speed -> Croc
mk pos dir speed' = Croc
  { __position = pos
  , __direction = dir
  , __linearMotion = LinearMotion.mk speed'
  , __headPhysical = mkHeadPhysical dir
  , __bodyPhysical = mkBodyPhysical dir
  , __bodySubmersible = Submersible.mk 1 1
  , __animated = Animated.mk CrocAnimation.Swimming CrocAnimation.asset
  }

mkHeadPhysical :: Direction -> Physical
mkHeadPhysical dir =
  case dir of
    East  -> Physical.mkAt (V2 129 1) headSize layer
    West  -> Physical.mkAt (V2 1 1) headSize layer
    North -> Physical.mkAt (V2 1 1) headSize layer
    South -> Physical.mkAt (V2 1 129) headSize layer
  where
    headSize = V2 62 62
    layer = Physical.Platform

mkBodyPhysical :: Direction -> Physical
mkBodyPhysical dir =
  case dir of
    East  -> Physical.mkAt (V2 1 1) bodyHorizontalSize layer
    West  -> Physical.mkAt (V2 65 1) bodyHorizontalSize layer
    North -> Physical.mkAt (V2 1 1) bodyVerticalSize layer
    South -> Physical.mkAt (V2 1 65) bodyVerticalSize layer
  where
    bodyHorizontalSize = V2 127 62
    bodyVerticalSize   = V2 62 127
    layer = Physical.Platform
