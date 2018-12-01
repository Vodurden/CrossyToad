module CrossyToad.Effect.Logger.Logger
  ( Logger(..)
  , logText
  , module CrossyToad.Effect.Logger.LogLevel
  ) where

import Control.Monad (when)
import Data.Set (Set)
import Data.Text (Text)

import CrossyToad.Effect.Logger.LogLevel

class Monad m => Logger m where
  -- | Get the enabled log levels.
  getEnabledLogLevels :: m (Set LogLevel)

  -- | Log any arbitrary text
  -- |
  -- | You should use logText instead
  logRaw :: Text -> m ()

-- | Logs a message at the given log level
-- |
-- | If the log level is not enabled the message will be ignored.
logText :: (Logger m) => LogLevel -> Text -> m ()
logText level text = do
  enabledLogLevels <- getEnabledLogLevels
  when (level `elem` enabledLogLevels) $ do
    let message = toPretty level <> " " <> text
    logRaw message
