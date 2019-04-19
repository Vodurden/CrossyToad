{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.RiverLog
  ( RiverLog(..)
  , HasRiverLog(..)
  , mk
  , step
  ) where

import           Control.Lens
import           Linear.V2

import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset
import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Physics
import           CrossyToad.Physics.LinearMotion (LinearMotion(..), HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Physics.Platform (Platform, HasPlatform(..))
import qualified CrossyToad.Physics.Platform as Platform
import           CrossyToad.Renderer.Sprite (Sprite(..), HasSprite(..))
import           CrossyToad.Time.TickSeconds

data RiverLog = RiverLog
  { __position :: !Position
  , __direction :: !Direction
  , __linearMotion :: !LinearMotion
  , __sprite :: !Sprite
  , __platform :: !Platform
  } deriving (Eq, Show)

makeClassy ''RiverLog

instance HasPosition RiverLog where position = _position
instance HasDirection RiverLog where direction = _direction
instance HasLinearMotion RiverLog where linearMotion = _linearMotion
instance HasSprite RiverLog where sprite = _sprite
instance HasPlatform RiverLog where platform = _platform

mk :: Position -> Direction -> RiverLog
mk pos dir = RiverLog
    { __position = pos
    , __direction = dir
    , __linearMotion = LinearMotion.mk logSpeed
    , __sprite = Sprite ImageAsset.Car (V2 64 64)
    , __platform = Platform.mkAt (V2 0 0) (V2 64 64)
    }
  where
    -- | How far the log moves in one second
    logSpeed :: Speed
    logSpeed = 64 * (1 / secondsPerTile)
      where secondsPerTile = 0.5

step :: HasRiverLog ent => TickSeconds -> ent -> ent
step (TickSeconds seconds) = riverLog %~ (LinearMotion.stepBy seconds)
