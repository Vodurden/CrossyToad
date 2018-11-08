module CrossyToad.Input.SDLInput
  ( stepInputIO
  , stepInput
  , getInputState
  , mkInputEvent
  ) where

import           Control.Lens
import           Control.Monad.State (MonadState)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe (catMaybes)
import           Data.Foldable (foldl')
import qualified SDL.Extended as SDL

import           CrossyToad.Input.InputEvent
import           CrossyToad.Input.InputState
import           CrossyToad.Input.KeyboardState
import           CrossyToad.Input.Key

stepInputIO :: (MonadState s m, MonadIO m, HasInputState s) => m ()
stepInputIO = do
    events <- SDL.pollEvents
    inputState %= stepInput (events)

getInputState :: (MonadState s m, HasInputState s) => m InputState
getInputState = use inputState

stepInput :: [SDL.Event] -> InputState -> InputState
stepInput events =
  let inputEvents' = catMaybes $ fmap mkInputEvent events
  in (inputEvents .~ inputEvents')
     . (inputState %~ updateInputState inputEvents')

updateInputState :: [InputEvent] -> InputState -> InputState
updateInputState events inputState' = foldl' (flip updateInputState') inputState' events
  where updateInputState' :: InputEvent -> InputState -> InputState
        updateInputState' (KeyPressed key) = inputState.keyboardState %~ (pressKey key)
        updateInputState' (KeyReleased key) = inputState.keyboardState %~ (releaseKey key)

mkInputEvent :: SDL.Event -> Maybe InputEvent
mkInputEvent event = case SDL.eventPayload event of
  (SDL.KeyboardEvent (SDL.KeyboardEventData _ motion _ keysym)) -> mkFromKeyboardEvent motion keysym
  _ -> Nothing

mkFromKeyboardEvent :: SDL.InputMotion -> SDL.Keysym -> Maybe InputEvent
mkFromKeyboardEvent SDL.Pressed keysym = KeyPressed <$> (mkKey keysym)
mkFromKeyboardEvent SDL.Released keysym = KeyReleased <$> (mkKey keysym)

mkKey :: SDL.Keysym -> Maybe Key
mkKey keysym = toKey $ SDL.keysymKeycode keysym

toKey :: SDL.Keycode -> Maybe Key
toKey SDL.KeycodeReturn = Just Return
toKey SDL.KeycodeEscape = Just Escape
toKey SDL.KeycodeW = Just W
toKey SDL.KeycodeA = Just A
toKey SDL.KeycodeS = Just S
toKey SDL.KeycodeD = Just D
toKey _ = Nothing
