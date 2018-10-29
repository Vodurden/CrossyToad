module CrossyToad where

import           Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import           Control.Monad.State (MonadState, StateT, evalStateT)
import           Control.Monad.IO.Class (MonadIO)
import           Linear.V2
import qualified SDL as SDL
import qualified SDL.Font as Font

import qualified CrossyToad.Assets as Assets
import           CrossyToad.Config (Config(..))
import           CrossyToad.Input.Input
import           CrossyToad.Effect.Renderer
import           CrossyToad.Effect.SDLRenderer
import           CrossyToad.Runner (mainLoop)
import           CrossyToad.Vars (Vars, initialVars)

main :: IO ()
main = do
  SDL.initializeAll
  Font.initialize
  window <- SDL.createWindow "Crossy Toad" SDL.defaultWindow
     { SDL.windowInitialSize = V2 800 600
     }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  assets <- Assets.loadAssets renderer
  let cfg = Config
            { _window = window
            , _renderer = renderer
            , __assets = assets
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
