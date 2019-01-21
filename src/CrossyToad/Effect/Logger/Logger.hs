{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module CrossyToad.Effect.Logger.Logger
  ( Logger(..)
  , logText
  , module CrossyToad.Effect.Logger.LogLevel
  ) where

import Control.Monad (when)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans, lift)
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

  -- | Default instances to easily derive our MonadTrans instances
  default getEnabledLogLevels :: (MonadTrans t, Logger m1, m ~ t m1) => m (Set LogLevel)
  getEnabledLogLevels = lift getEnabledLogLevels

  default logRaw :: (MonadTrans t, Logger m1, m ~ t m1) => Text -> m ()
  logRaw = lift . logRaw

instance Logger m => Logger (StateT s m)
instance Logger m => Logger (ReaderT s m)

-- | Logs a message at the given log level
-- |
-- | If the log level is not enabled the message will be ignored.
logText :: (Logger m) => LogLevel -> Text -> m ()
logText level text = do
  enabledLogLevels <- getEnabledLogLevels
  when (level `elem` enabledLogLevels) $ do
    let message = toPretty level <> " " <> text
    logRaw message
