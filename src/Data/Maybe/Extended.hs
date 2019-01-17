module Data.Maybe.Extended
  ( module Data.Maybe
  , guardMaybe
  , whenJust
  ) where

import           Control.Applicative (Alternative, empty)
import           Data.Maybe

-- | guardMaybe exits the computation when given Nothing or returns the value
-- | when given Just
guardMaybe :: (Alternative m) => Maybe a -> m a
guardMaybe (Just a) = pure a
guardMaybe Nothing = empty

whenJust :: (Applicative f) => Maybe a -> (a -> f ()) -> f ()
whenJust (Just a) f = f a
whenJust Nothing _ = pure ()
