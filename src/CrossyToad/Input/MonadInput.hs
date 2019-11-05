{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | The @Input@ feature is responsible for gathering input from a human
-- | and making it available to the rest of the application.
-- |
-- | Crucially the @Input@ feature is not responsible for understanding the
-- | intent of the user, it only gathers input such that other features can
-- | interpret it.
module CrossyToad.Input.MonadInput
  ( MonadInput(..)
  ) where

import Control.Monad.State.Strict (StateT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans, lift)

import CrossyToad.Input.Intents (Intents)

class Monad m => MonadInput m where
  tickInput :: m Intents

  default tickInput :: (MonadTrans t, MonadInput m1, m ~ t m1) => m Intents
  tickInput = lift tickInput

instance MonadInput m => MonadInput (StateT s m)
instance MonadInput m => MonadInput (ReaderT s m)
