{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.RiverLog
  ( RiverLog(..)
  , HasRiverLog(..)
  , mk
  , stepAll
  , step
  , render
  ) where

import           Control.Lens
import           Control.Monad.Reader (MonadReader)
import           Linear.V2

import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset
import           CrossyToad.Renderer.RenderCommand (RenderCommand)
import           CrossyToad.Geometry.Position
import           CrossyToad.Physics.Physics
import           CrossyToad.Physics.LinearMotion (LinearMotion(..), HasLinearMotion(..))
import qualified CrossyToad.Physics.LinearMotion as LinearMotion
import           CrossyToad.Renderer.Sprite (Sprite(..), HasSprite(..))
import qualified CrossyToad.Renderer.Sprite as Sprite
import           CrossyToad.Time.TickSeconds

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

stepAll :: (MonadReader TickSeconds m, HasRiverLogs ent) => ent -> m ent
stepAll = (riverLogs.traverse) step

step :: (MonadReader TickSeconds m, HasRiverLog ent) => ent -> m ent
step = mapMOf riverLog LinearMotion.step

render :: RiverLog -> RenderCommand
render = Sprite.render
