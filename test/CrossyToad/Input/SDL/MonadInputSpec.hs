module CrossyToad.Input.SDL.MonadInputSpec where

import           Test.Tasty.Hspec

import           Control.Arrow ((>>>))
import           Control.Lens
import qualified Data.Set as Set
import qualified SDL.Extended as SDL

import           CrossyToad.Input.Key
import           CrossyToad.Input.KeyboardState
import           CrossyToad.Input.InputEvent
import           CrossyToad.Input.InputState
import           CrossyToad.Input.MonadInput.SDL.MonadInput

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

-- | TODO: Consider using a property based test generator here
describeKey :: SDL.Keycode -> Key -> SpecWith ()
describeKey keycode expectedKey = do
  let sdlPressEvent = (mkKeyboardEvent SDL.Pressed keycode)
  let sdlReleaseEvent = (mkKeyboardEvent SDL.Released keycode)

  context ("when " ++ (show expectedKey) ++ " pressed") $ do
    let inputState' = stepInputState [sdlPressEvent] initialInputState

    it ("should have a 'KeyPressed' input event") $ do
      (inputState' ^. inputEvents) `shouldBe` [KeyPressed expectedKey]

    it ("should be in the pressed keyboard state") $ do
      (inputState' ^. keyboardState.pressed) `shouldBe` (Set.singleton expectedKey)

  context ("when " ++ (show expectedKey) ++ " released") $ do
    let inputState' = stepInputState [sdlReleaseEvent] initialInputState

    it ("should have a 'Released' input event") $ do
      (inputState' ^. inputEvents) `shouldBe` [KeyReleased expectedKey]

    it ("should not be in the pressed keyboard state") $ do
      (inputState' ^. keyboardState.pressed) `shouldBe` (Set.empty)

  context ("when " ++ (show expectedKey) ++ " pressed and released in the same frame") $ do
    let inputState' = stepInputState [sdlPressEvent, sdlReleaseEvent] initialInputState

    it ("should have a 'KeyPressed' and 'KeyReleased' input event") $ do
      (inputState' ^. inputEvents) `shouldBe` [KeyPressed expectedKey, KeyReleased expectedKey]

    it ("should not be pressed in the keyboard state") $ do
      (inputState' ^. keyboardState.pressed) `shouldBe` (Set.empty)

  context ("when " ++ (show expectedKey) ++ " pressed and released across frames") $ do
    let inputState' = (stepInputState [sdlPressEvent] >>> stepInputState [sdlReleaseEvent]) initialInputState

    it ("should have a 'KeyReleased' input event") $ do
      (inputState' ^. inputEvents) `shouldBe` [KeyReleased expectedKey]

    it ("should not be pressed in the keyboard state") $ do
      (inputState' ^. keyboardState.pressed) `shouldBe` (Set.empty)

spec_Input_SDL_Input :: Spec
spec_Input_SDL_Input =
  describe "stepInput" $ do
    describeKey SDL.KeycodeReturn Return
    describeKey SDL.KeycodeEscape Escape
    describeKey SDL.KeycodeW W
    describeKey SDL.KeycodeA A
    describeKey SDL.KeycodeS S
    describeKey SDL.KeycodeD D
