{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module CrossyToad.Logger.MonadLogger
  ( MonadLogger(..)
  , logText
  , logShow
  ) where

import           Control.Monad (when)
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Reader (ReaderT)
import           Control.Monad.Trans (MonadTrans, lift)
import           Data.Set (Set)
import           Data.Text (Text)
import qualified Data.Text as Text

import           CrossyToad.Logger.LogLevel
import           CrossyToad.Time.Task

class Monad m => MonadLogger m where
  -- | Get the enabled log levels.
  getEnabledLogLevels :: m (Set LogLevel)

  -- | Log any arbitrary text
  -- |
  -- | You should use logText instead
  logRaw :: Text -> m ()

  -- | Default instances to easily derive our MonadTrans instances
  default getEnabledLogLevels :: (MonadTrans t, MonadLogger m1, m ~ t m1) => m (Set LogLevel)
  getEnabledLogLevels = lift getEnabledLogLevels

  default logRaw :: (MonadTrans t, MonadLogger m1, m ~ t m1) => Text -> m ()
  logRaw = lift . logRaw

instance MonadLogger m => MonadLogger (StateT s m)
instance MonadLogger m => MonadLogger (ReaderT s m)
instance MonadLogger m => MonadLogger (Task m)

-- | Logs a message at the given log level
-- |
-- | If the log level is not enabled the message will be ignored.
logText :: (MonadLogger m) => LogLevel -> Text -> m ()
logText level text = do
  enabledLogLevels <- getEnabledLogLevels
  when (level `elem` enabledLogLevels) $ do
    let message = toPretty level <> " " <> text
    logRaw message

logShow :: (MonadLogger m, Show a) => LogLevel -> a -> m ()
logShow level a = logText level (Text.pack $ show a)
