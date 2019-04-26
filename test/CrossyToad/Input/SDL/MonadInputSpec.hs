module CrossyToad.Input.SDL.MonadInputSpec where

import           Test.Tasty.Hspec

import           Control.Arrow ((>>>))
import qualified SDL.Extended as SDL

import           CrossyToad.Input.Intent (Intent)
import qualified CrossyToad.Input.Intent as Intent
import qualified CrossyToad.Input.Intents as Intents
import           CrossyToad.Input.MonadInput.SDL.MonadInput
import           CrossyToad.Physics.Direction (Direction(..))

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
describeIntent :: String -> SDL.Keycode -> Intent -> SpecWith ()
describeIntent keyname keycode expectedIntent = do
  let sdlPressEvent = (mkKeyboardEvent SDL.Pressed keycode)
  let sdlReleaseEvent = (mkKeyboardEvent SDL.Released keycode)

  it ("when " ++ keyname ++ " is pressed it should intend to " ++ (show expectedIntent) ++ " once") $ do
    let intents' = tickIntents [sdlPressEvent] Intents.initialize
    Intents.once intents' `shouldBe` [expectedIntent]

  it ("when " ++ keyname ++ " is released it should not intend to " ++ (show expectedIntent)) $ do
    let intents' = tickIntents [sdlReleaseEvent] Intents.initialize
    Intents.all intents' `shouldBe` []

  it ("when " ++ keyname ++ " pressed and released in the same frame it should not intend to " ++ (show expectedIntent)) $ do
    let intents' = tickIntents [sdlPressEvent, sdlReleaseEvent] Intents.initialize
    Intents.all intents' `shouldBe` []

  it ("when " ++ keyname ++ " pressed and released across frames it should not intend to " ++ (show expectedIntent)) $ do
    let intents' = (tickIntents [sdlPressEvent] >>> tickIntents [sdlReleaseEvent]) Intents.initialize
    Intents.all intents' `shouldBe` []

spec_Input_SDL_Input :: Spec
spec_Input_SDL_Input =
  describe "tickIntents" $ do
    describeIntent "Return" SDL.KeycodeReturn Intent.EnterOrConfirm
    describeIntent "Escape" SDL.KeycodeEscape Intent.PauseOrExit
    describeIntent "W" SDL.KeycodeW (Intent.Move North)
    describeIntent "A" SDL.KeycodeA (Intent.Move West)
    describeIntent "S" SDL.KeycodeS (Intent.Move South)
    describeIntent "D" SDL.KeycodeD (Intent.Move East)
