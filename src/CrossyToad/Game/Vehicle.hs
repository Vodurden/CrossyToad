{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

module CrossyToad.Game.Vehicle
  ( Vehicle(..)
  , HasVehicle(..)
  , Car
  , Truck
  , WoodLog
  , mkCar
  , mkTruck
  , mkWoodLog
  ) where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.LinearMotion (LinearMotion(..), HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Physics.Physical (Physical, HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Physics.Physics
import           CrossyToad.Renderer.Animated (Animated(..), HasAnimated(..))
import qualified CrossyToad.Renderer.Animated as Animated
import qualified CrossyToad.Renderer.Asset.Animation.Car as CarAnimation
import qualified CrossyToad.Renderer.Asset.Animation.Truck as TruckAnimation
import qualified CrossyToad.Renderer.Asset.Animation.WoodLog as WoodLogAnimation

data Vehicle key = Vehicle
  { __position :: !Position
  , __direction :: !Direction
  , __linearMotion :: !LinearMotion
  , __physical :: !Physical
  , __animated :: !(Animated key)
  } deriving (Eq, Show)

type Car = Vehicle CarAnimation.Animation
type Truck = Vehicle TruckAnimation.Animation
type WoodLog = Vehicle WoodLogAnimation.Animation

makeClassy ''Vehicle

instance HasPosition (Vehicle key) where position = _position
instance HasDirection (Vehicle key) where direction = _direction
instance HasLinearMotion (Vehicle key) where linearMotion = _linearMotion
instance HasPhysical (Vehicle key) where physical = _physical
instance HasAnimated (Vehicle key) key where animated = _animated

mkCar :: Position -> Direction -> Speed -> Car
mkCar pos dir speed' = Vehicle
    { __position = pos
    , __direction = dir
    , __linearMotion = LinearMotion.mk speed'
    , __physical = Physical.mkAt (V2 1 1) (V2 62 62) Physical.Ground
    , __animated = Animated.mk CarAnimation.DriveLeft CarAnimation.asset
    }

mkTruck :: Position -> Direction -> Speed -> Truck
mkTruck pos dir speed' = Vehicle
    { __position = pos
    , __direction = dir
    , __linearMotion = LinearMotion.mk speed'
    , __physical = Physical.mkAt (V2 1 1) (V2 127 62) Physical.Ground
    , __animated = Animated.mk TruckAnimation.DriveLeft TruckAnimation.asset
    }

mkWoodLog :: Position -> Direction -> Speed -> WoodLog
mkWoodLog pos dir speed' = Vehicle
    { __position = pos
    , __direction = dir
    , __linearMotion = LinearMotion.mk speed'
    , __physical = Physical.mkAt (V2 1 1) (V2 62 62) Physical.Platform
    , __animated = Animated.mk WoodLogAnimation.FloatLeft WoodLogAnimation.asset
    }
