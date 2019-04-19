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
  , getKeyboardState
  , getInputEvents
  ) where

import Control.Lens
import Control.Monad.State (StateT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans, lift)

import CrossyToad.Input.InputEvent
import CrossyToad.Input.InputState
import CrossyToad.Input.KeyboardState

class Monad m => MonadInput m where
  tickInput :: m ()
  getInputState :: m InputState

  default tickInput :: (MonadTrans t, MonadInput m1, m ~ t m1) => m ()
  tickInput = lift tickInput

  default getInputState :: (MonadTrans t, MonadInput m1, m ~ t m1) => m InputState
  getInputState = lift getInputState

instance MonadInput m => MonadInput (StateT s m)
instance MonadInput m => MonadInput (ReaderT s m)

getKeyboardState :: (MonadInput m) => m KeyboardState
getKeyboardState = (view keyboardState) <$> getInputState

getInputEvents :: (MonadInput m) => m [InputEvent]
getInputEvents = (view inputEvents) <$> getInputState
