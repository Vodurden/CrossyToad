{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.RiverLog
  ( RiverLog(..)
  , HasRiverLog(..)
  , mk
  , stepAll
  , step
  , render
  ) where

import           Control.Lens
import           Linear.V2

import qualified CrossyToad.Effect.Renderer.ImageAsset as ImageAsset
import           CrossyToad.Effect.Renderer.RenderCommand (RenderCommand)
import           CrossyToad.Time.MonadTime
import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Physics
import           CrossyToad.Physics.LinearMotion (LinearMotion(..), HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Sprite.Sprite (Sprite(..), HasSprite(..))
import qualified CrossyToad.Sprite.Sprite as Sprite

data RiverLog = RiverLog
  { __position :: !Position
  , __direction :: !Direction
  , __linearMotion :: !LinearMotion
  , __sprite :: !Sprite
  } deriving (Eq, Show)

makeClassy ''RiverLog

class HasRiverLogs a where
  riverLogs :: Lens' a [RiverLog]

instance HasRiverLogs [RiverLog] where
  riverLogs = id

instance HasPosition RiverLog where
  position = _position

instance HasDirection RiverLog where
  direction = _direction

instance HasLinearMotion RiverLog where
  linearMotion = _linearMotion

instance HasSprite RiverLog where
  sprite = _sprite

mk :: Position -> Direction -> RiverLog
mk pos dir = RiverLog
    { __position = pos
    , __direction = dir
    , __linearMotion = LinearMotion.mk logSpeed
    , __sprite = Sprite ImageAsset.Car (V2 64 64)
    }
  where
    -- | How far the log moves in one second
    logSpeed :: Speed
    logSpeed = 64 * (1 / secondsPerTile)
      where secondsPerTile = 0.5

stepAll :: (MonadTime m, HasRiverLogs ent) => ent -> m ent
stepAll = (riverLogs.traverse) step

step :: (MonadTime m, HasRiverLog ent) => ent -> m ent
step = mapMOf riverLog LinearMotion.step

render :: RiverLog -> RenderCommand
render = Sprite.render
