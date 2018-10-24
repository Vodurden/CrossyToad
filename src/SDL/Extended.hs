module SDL.Extended
  ( module SDL
  , keyPressed
  , keyReleased
  , quitEvent
  ) where

import Control.Lens
import SDL

keyPressed :: (HasEventPayload ep) => Keycode -> ep -> Bool
keyPressed = keyMotion SDL.Pressed

keyReleased :: (HasEventPayload ep) => SDL.Keycode -> ep -> Bool
keyReleased = keyMotion SDL.Released

keyMotion :: (HasEventPayload ep) => SDL.InputMotion -> SDL.Keycode -> ep -> Bool
keyMotion expectedMotion expectedKeycode ep =
  case (view SDL.Extended.eventPayload ep) of
    (SDL.KeyboardEvent event) ->
      let motion = SDL.keyboardEventKeyMotion event
          keycode = (SDL.keysymKeycode $ SDL.keyboardEventKeysym event)
      in keycode == expectedKeycode && motion == expectedMotion
    _ -> False

quitEvent :: (HasEventPayload ep) => ep -> Bool
quitEvent ep = case (view SDL.Extended.eventPayload ep) of
  SDL.QuitEvent -> True
  _ -> False

-- SDL Lenses
class HasEventPayload s where
  eventPayload :: Lens' s SDL.EventPayload

instance HasEventPayload SDL.EventPayload where
  eventPayload = lens id (\_ s -> s)

instance HasEventPayload SDL.Event where
  eventPayload = lens SDL.eventPayload (\v s -> v { SDL.eventPayload = s })
