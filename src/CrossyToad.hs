module CrossyToad where

import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import           Control.Monad.State (MonadState, StateT, evalStateT)
import           Control.Monad.IO.Class (MonadIO)
import qualified SDL as SDL

import CrossyToad.Config
import CrossyToad.State
import CrossyToad.Runner (mainLoop)
import CrossyToad.Effect.Input
import CrossyToad.Effect.Renderer

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Crossy Toad" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  let cfg = Config
            { cWindow = window
            , cRenderer = renderer
            }
  runCrossyToad cfg initialVars mainLoop

newtype CrossyToad a = CrossyToad (ReaderT Config (StateT Vars IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO)

runCrossyToad :: Config -> Vars -> CrossyToad a -> IO a
runCrossyToad config vars (CrossyToad m) = evalStateT (runReaderT m config) vars

instance Input CrossyToad where
  updateInput = updateInput'
  setInput = setInput'
  getInput = getInput'

instance Renderer CrossyToad where
  clearScreen = clearScreen'
  drawScreen = drawScreen'

message :: String
message = "Hello, Crossy Toad!"
