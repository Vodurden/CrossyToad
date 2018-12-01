module CrossyToad.Effect.Logger.IO.Logger where

import           Control.Lens
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text.IO as Text

import CrossyToad.Effect.Logger.LogLevel
import CrossyToad.Effect.Logger.IO.Env

getEnabledLogLevels ::
  ( MonadReader r m
  , HasEnv r
  ) => m (Set LogLevel)
getEnabledLogLevels = view (env.logLevels)

-- | Log messages to stdout
logRawStdout :: (MonadIO m) => Text -> m ()
logRawStdout = liftIO . Text.putStrLn
