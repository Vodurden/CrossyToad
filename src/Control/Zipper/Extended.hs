module Control.Zipper.Extended
  ( module Control.Zipper
  , wrapLeft
  , wrapRight
  ) where

import Control.Monad (liftM2)
import Control.Zipper
import Data.Maybe (fromMaybe)

wrapLeft :: (a :>> b) -> (a :>> b)
wrapLeft = liftM2 fromMaybe rightmost leftward

wrapRight :: (a :>> b) -> (a :>> b)
wrapRight = liftM2 fromMaybe leftmost rightward
