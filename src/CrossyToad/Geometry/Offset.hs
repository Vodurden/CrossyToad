{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

module CrossyToad.Geometry.Offset where

import Control.Lens

import CrossyToad.Geometry.Internal

class HasOffset s u where
  offset :: Lens' s (Offset u)

instance HasOffset (Offset u) u where
  offset = id
