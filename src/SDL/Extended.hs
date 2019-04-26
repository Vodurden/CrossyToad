{-# LANGUAGE TemplateHaskell #-}

module SDL.Extended
  ( module SDL
  , timestamp
  , payload
  , window
  , keyMotion
  , repeatL
  , keysym
  , scancode
  , keycode
  , modifier
  ) where

import Control.Lens
import SDL

-- SDL Lenses
makeLensesFor
  [ ("eventTimestamp", "timestamp")
  , ("eventPayload", "payload")
  ] ''SDL.Event

makeLensesFor
  [ ("keyboardEventWindow", "window")
  , ("keyboardEventKeyMotion", "keyMotion")
  , ("keyboardEventRepeat", "repeatL")
  , ("keyboardEventKeysym", "keysym")
  ] ''SDL.KeyboardEventData

makeLensesFor
  [ ("keysymScancode", "scancode")
  , ("keysymKeycode", "keycode")
  , ("keysymModifier", "modifier")
  ] '' SDL.Keysym
