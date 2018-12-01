{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | The @Renderer@ feature is responsible for drawing the screen.
-- |
-- | In particular a renderer knows how to "draw" every visual artifact in
-- | the game such that a human will see it.
module CrossyToad.Effect.Renderer.Renderer where

import           Control.Monad.State (StateT)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.Trans (MonadTrans, lift)
import           Linear.V2

import           CrossyToad.Effect.Renderer.Asset (Asset)
import qualified CrossyToad.Effect.Renderer.Asset as Asset

class Monad m => Renderer m where
  clearScreen :: m ()
  drawScreen :: m ()

  draw :: Asset -> V2 Float -> m ()

  default clearScreen :: (MonadTrans t, Renderer m1, m ~ t m1) => m ()
  clearScreen = lift clearScreen
  default drawScreen :: (MonadTrans t, Renderer m1, m ~ t m1) => m ()
  drawScreen = lift drawScreen
  default draw :: (MonadTrans t, Renderer m1, m ~ t m1) => Asset -> V2 Float -> m ()
  draw asset pos = lift (draw asset pos)

instance Renderer m => Renderer (StateT s m)
instance Renderer m => Renderer (ReaderT s m)

drawTitleText :: (Renderer m) => V2 Float -> m ()
drawTitleText = draw Asset.TitleSprite

drawToad :: (Renderer m) => V2 Float -> m ()
drawToad = draw Asset.Toad

drawToad2 :: (Renderer m) => V2 Float -> m ()
drawToad2 = draw Asset.Toad2

drawCar :: (Renderer m) => V2 Float -> m ()
drawCar = draw Asset.Car
