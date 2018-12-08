{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

module CrossyToad.Geometry.Dimensions where

import Control.Lens

import CrossyToad.Geometry.Internal

class HasDimensions s u where
  dimensions :: Lens' s (Dimensions u)

instance HasDimensions (Dimensions u) u where
  dimensions = id
