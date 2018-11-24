{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | The @Input@ feature is responsible for gathering input from a human
-- | and making it available to the rest of the application.
-- |
-- | Crucially the @Input@ feature is not responsible for understanding the
-- | intent of the user, it only gathers input such that other features can
-- | interpret it.
module CrossyToad.Input.Input
  ( Input(..)
  , getKeyboardState
  , getInputEvents
  , module CrossyToad.Input.InputState
  , module CrossyToad.Input.InputEvent
  , module CrossyToad.Input.Key
  , module CrossyToad.Input.KeyboardState
  ) where

import Control.Lens
import Control.Monad.State (StateT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (MonadTrans, lift)

import CrossyToad.Input.InputEvent
import CrossyToad.Input.InputState
import CrossyToad.Input.KeyboardState
import CrossyToad.Input.Key

class Monad m => Input m where
  stepInput :: m ()
  getInputState :: m InputState

  default stepInput :: (MonadTrans t, Input m1, m ~ t m1) => m ()
  stepInput = lift stepInput

  default getInputState :: (MonadTrans t, Input m1, m ~ t m1) => m InputState
  getInputState = lift getInputState

instance Input m => Input (StateT s m)
instance Input m => Input (ReaderT s m)

getKeyboardState :: (Input m) => m KeyboardState
getKeyboardState = (view keyboardState) <$> getInputState

getInputEvents :: (Input m) => m [InputEvent]
getInputEvents = (view inputEvents) <$> getInputState
