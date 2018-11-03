module CrossyToad.Input.SDLInput
  ( pollInput
  , mkInputEvent
  ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Maybe (catMaybes)
import qualified SDL.Extended as SDL

import           CrossyToad.Input.InputEvent

pollInput :: (MonadIO m) => m [InputEvent]
pollInput = do
  events <- SDL.pollEvents
  pure $ catMaybes $ fmap mkInputEvent events

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
