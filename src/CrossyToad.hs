module CrossyToad where

import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import           Control.Monad.State (MonadState, StateT, evalStateT)
import           Control.Monad.IO.Class (MonadIO)
import           Linear.V2
import qualified SDL as SDL
import qualified SDL.Font as Font

import CrossyToad.Assets
import CrossyToad.Config
import CrossyToad.Runner (mainLoop)
import CrossyToad.State
import CrossyToad.Effect.Input
import CrossyToad.Effect.Renderer
import CrossyToad.Effect.SDLRenderer

main :: IO ()
main = do
  SDL.initializeAll
  Font.initialize
  window <- SDL.createWindow "Crossy Toad" SDL.defaultWindow
     { SDL.windowInitialSize = V2 800 600
     }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  assets <- loadAssets renderer
  let cfg = Config
            { cWindow = window
            , cRenderer = renderer
            , cAssets = assets
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

  drawTitleText = drawTitleText'

instance SDLRenderer CrossyToad where
  presentRenderer = presentRenderer'
  clearRenderer = clearRenderer'
  queryTexture = queryTexture'
  drawTexture = drawTexture'

message :: String
message = "Hello, Crossy Toad!"
