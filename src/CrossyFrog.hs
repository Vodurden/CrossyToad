module CrossyFrog where

import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import           Control.Monad.State (MonadState, StateT, evalStateT)
import           Control.Monad.IO.Class (MonadIO)
import qualified SDL as SDL

import CrossyFrog.Config
import CrossyFrog.State
import CrossyFrog.Runner (mainLoop)
import CrossyFrog.Effect.Input
import CrossyFrog.Effect.Renderer

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Crossy Frog" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  let cfg = Config
            { cWindow = window
            , cRenderer = renderer
            }
  runCrossyFrog cfg initialVars mainLoop

newtype CrossyFrog a = CrossyFrog (ReaderT Config (StateT Vars IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO)

runCrossyFrog :: Config -> Vars -> CrossyFrog a -> IO a
runCrossyFrog config vars (CrossyFrog m) = evalStateT (runReaderT m config) vars

instance Input CrossyFrog where
  updateInput = updateInput'
  setInput = setInput'
  getInput = getInput'

instance Renderer CrossyFrog where
  clearScreen = clearScreen'
  drawScreen = drawScreen'

message :: String
message = "Hello, Crossy Frog!"
