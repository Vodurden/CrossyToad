{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Zipper.Extended
  ( module Control.Zipper
  , wrapLeft
  , wrapRight
  ) where

import Control.Monad (liftM2)
import Control.Zipper
import Control.Lens
import Data.Maybe (fromMaybe)

wrapLeft :: (a :>> b) -> (a :>> b)
wrapLeft = liftM2 fromMaybe rightmost leftward

wrapRight :: (a :>> b) -> (a :>> b)
wrapRight = liftM2 fromMaybe leftmost rightward

-- TODO: Consider whether these instances are lawful and whether
--       we can/should avoid orphan instances.
--
--       Also consider removing Show entirely in favor of Pretty
instance (Eq b) => Eq (a :>> b) where
  (==) a b = a^.focus == b^.focus

instance (Show b) => Show (a :>> b) where
  show b = show $ b^.focus
