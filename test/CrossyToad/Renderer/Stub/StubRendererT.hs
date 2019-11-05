module CrossyToad.Renderer.Stub.StubRendererT where

import Control.Monad.Trans (MonadTrans)
import Control.Monad.Writer.Strict (WriterT, execWriterT, tell)

import CrossyToad.Renderer.MonadRenderer
import CrossyToad.Renderer.Stub.RenderCommand

newtype StubRendererT m a = StubRendererT (WriterT [RenderCommand] m a)
  deriving (Functor, Applicative, Monad, MonadTrans)

runStubRendererT :: (Monad m) => StubRendererT m a -> m [RenderCommand]
runStubRendererT (StubRendererT x) = execWriterT x

instance Monad m => MonadRenderer (StubRendererT m) where
  clearScreen =
    StubRendererT $ tell [ClearScreen]

  drawScreen =
    StubRendererT $ tell [DrawScreen]

  draw asset tClip sClip degrees flip' =
    StubRendererT $ tell [Draw asset tClip sClip degrees flip']

  drawAt asset pos =
    StubRendererT $ tell [DrawAt asset pos]

  drawRect pos =
    StubRendererT $ tell [DrawRect pos]

  drawText asset degrees tClip sClip colour text =
    StubRendererT $ tell [DrawText asset degrees tClip sClip colour text]
