module CrossyToad.Input.SDLInputSpec where

import           Test.Tasty.Hspec

import           Control.Arrow ((>>>))
import           Control.Lens
import qualified SDL.Extended as SDL

import CrossyToad.Input.Input
import CrossyToad.Input.SDLInput

mkEvent :: SDL.EventPayload -> SDL.Event
mkEvent payload = SDL.Event { eventTimestamp = undefined , eventPayload = payload }

mkKeyboardEvent :: SDL.InputMotion -> SDL.Keycode -> SDL.Event
mkKeyboardEvent motion keycode = mkEvent $ SDL.KeyboardEvent $ SDL.KeyboardEventData
  { keyboardEventWindow = Nothing
  , keyboardEventKeyMotion = motion
  , keyboardEventRepeat = False
  , keyboardEventKeysym = mkKeysym keycode
  }

mkKeysym :: SDL.Keycode -> SDL.Keysym
mkKeysym code = SDL.Keysym
  { keysymScancode = undefined
  , keysymKeycode = code
  , keysymModifier = undefined
  }

describeKey :: SDL.Keycode -> Key -> SpecWith ()
describeKey keycode expectedKey = do
  describeKeyPressed keycode expectedKey
  describeKeyReleased keycode expectedKey

describeKeyPressed :: SDL.Keycode -> Key -> SpecWith ()
describeKeyPressed keycode expectedKey =
  it ("should be '" ++ (show expectedKey) ++ "' when pressed") $ do
    let pressKey = mkInputEvent (mkKeyboardEvent SDL.Pressed keycode)
    pressKey `shouldBe` (Just $ KeyPressed expectedKey)

describeKeyReleased :: SDL.Keycode -> Key -> SpecWith ()
describeKeyReleased keycode expectedKey =
  it ("should be '" ++ (show expectedKey) ++ "' when released") $ do
    let pressKey = mkInputEvent (mkKeyboardEvent SDL.Released keycode)
    pressKey `shouldBe` (Just $ KeyReleased expectedKey)

spec_Input_SDLInput :: Spec
spec_Input_SDLInput =
  describe "mkInputEvent" $ do
    describeKey SDL.KeycodeReturn Return
    describeKey SDL.KeycodeEscape Escape
    describeKey SDL.KeycodeW W
    describeKey SDL.KeycodeA A
    describeKey SDL.KeycodeS S
    describeKey SDL.KeycodeD D
