module CrossyToad where

import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import           Control.Monad.State (MonadState, StateT, evalStateT)
import           Control.Monad.IO.Class (MonadIO)

import           CrossyToad.Env (Env(..))
import           CrossyToad.Input.Input
import qualified CrossyToad.Input.SDLInput as SDLInput
import           CrossyToad.Renderer.Renderer
import qualified CrossyToad.Renderer.SDL.SDL as SDLRenderer
import           CrossyToad.Time.Time
import qualified CrossyToad.Time.SDL.Time as SDLTime
import           CrossyToad.Vars (Vars)

newtype CrossyToad a = CrossyToad (ReaderT Env (StateT Vars IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadState Vars, MonadIO)

runCrossyToad :: Env -> Vars -> CrossyToad a -> IO a
runCrossyToad config vars (CrossyToad m) = evalStateT (runReaderT m config) vars

instance Input CrossyToad where
  stepInput = SDLInput.stepInputIO
  getInputState = SDLInput.getInputState

instance Renderer CrossyToad where
  clearScreen = SDLRenderer.clearScreen
  drawScreen = SDLRenderer.drawScreen

  draw = SDLRenderer.draw

instance Time CrossyToad where
  stepTime = SDLTime.stepTime
  deltaTime = SDLTime.deltaTime
