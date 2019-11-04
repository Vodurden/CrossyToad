module Data.Bounded.ASCIIChar
  ( ASCIIChar(..)
  , unASCIIChar
  ) where

newtype ASCIIChar = ASCIIChar { unASCIIChar :: Char }
  deriving (Eq, Show, Enum)

instance Bounded ASCIIChar where
  minBound = ASCIIChar 'A'
  maxBound = ASCIIChar 'Z'
