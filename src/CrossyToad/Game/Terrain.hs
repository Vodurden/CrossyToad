{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.Terrain
  ( Terrain(..)
  , HasTerrain(..)
  , mkGrass
  , mkRoad
  , mkSwamp
  , mkWater
  ) where

import           Control.Lens
import           Linear.V2

import           CrossyToad.Geometry.Position (Position, HasPosition(..))
import           CrossyToad.Physics.Physical (Physical, HasPhysical(..))
import qualified CrossyToad.Physics.Physical as Physical
import           CrossyToad.Renderer.Animated (Animated(..), HasAnimated(..))
import qualified CrossyToad.Renderer.Animated as Animated
import qualified CrossyToad.Renderer.Asset.Animation.Terrain as TerrainAnimation

data Terrain = Terrain
  { __position :: !Position
  , __physical :: !Physical
  , __animated :: !(Animated TerrainAnimation.Animation)
  } deriving (Eq, Show)

makeClassy ''Terrain

instance HasPosition Terrain where position = _position
instance HasPhysical Terrain where physical = _physical
instance HasAnimated Terrain TerrainAnimation.Animation where animated = _animated

mkGrass :: Position -> Terrain
mkGrass pos = Terrain
  { __position = pos
  , __physical = Physical.mkAt (V2 1 1) (V2 62 62) Physical.Ground
  , __animated = Animated.mk TerrainAnimation.Grass TerrainAnimation.asset
  }

mkRoad :: Position -> Terrain
mkRoad pos = Terrain
  { __position = pos
  , __physical = Physical.mkAt (V2 1 1) (V2 62 62) Physical.Ground
  , __animated = Animated.mk TerrainAnimation.Road TerrainAnimation.asset
  }

mkSwamp :: Position -> Terrain
mkSwamp pos = Terrain
  { __position = pos
  , __physical = Physical.mkAt (V2 1 1) (V2 62 62) Physical.Ground
  , __animated = Animated.mk TerrainAnimation.Swamp TerrainAnimation.asset
  }

mkWater :: Position -> Terrain
mkWater pos = Terrain
  { __position = pos
  , __physical = Physical.mkAt (V2 1 1) (V2 62 62) Physical.Ground
  , __animated = Animated.mk TerrainAnimation.Water TerrainAnimation.asset
  }
