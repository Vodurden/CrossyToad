{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Effect.Input.KeyboardState where

import           Control.Lens
import           Data.Set (Set)
import qualified Data.Set as Set

import           CrossyToad.Effect.Input.Key

data KeyboardState = KeyboardState
  { _pressed :: Set Key
  } deriving (Eq, Show)

makeClassy ''KeyboardState

initialKeyboardState :: KeyboardState
initialKeyboardState = KeyboardState
  { _pressed = Set.empty
  }

pressKey :: Key -> KeyboardState -> KeyboardState
pressKey key = pressed %~ (Set.insert key)

releaseKey :: Key -> KeyboardState -> KeyboardState
releaseKey key = pressed %~ (Set.delete key)

keyDown :: KeyboardState -> Key -> Bool
keyDown ks key = key `elem` (ks^.pressed)
