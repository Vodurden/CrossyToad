-- | The @Input@ feature is responsible for gathering input from a human
-- | and making it available to the rest of the application.
-- |
-- | Crucially the @Input@ feature is not responsible for understanding the
-- | intent of the user, it only gathers input such that other features can
-- | interpret it.
module CrossyToad.Input.Input
  ( Input(..)
  , InputEvent(..)
  , Key(..)
  ) where

import CrossyToad.Input.InputEvent

class Monad m => Input m where
  pollInput :: m [InputEvent]
