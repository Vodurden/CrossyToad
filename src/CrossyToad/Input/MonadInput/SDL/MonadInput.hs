module CrossyToad.Input.MonadInput.SDL.MonadInput
  ( tickInput
  , tickIntents
  ) where

import           Control.Lens
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe (catMaybes)
import           Data.IORef (readIORef, modifyIORef')
import           SDL.Extended (payload, keyMotion, keysym, keycode)
import qualified SDL.Extended as SDL

import           CrossyToad.Input.Intents (Intents)
import qualified CrossyToad.Input.Intents as Intents
import           CrossyToad.Input.Intent (Intent)
import qualified CrossyToad.Input.Intent as Intent
import           CrossyToad.Input.IntentEvent (IntentEvent)
import qualified CrossyToad.Input.IntentEvent as IntentEvent
import           CrossyToad.Physics.Direction (Direction(..))
import           CrossyToad.Input.MonadInput.SDL.Env

tickInput ::
  ( MonadReader r m
  , HasEnv r
  , MonadIO m
  ) => m Intents
tickInput = do
  intentsRef' <- view (env.intentsRef)
  events <- SDL.pollEvents
  liftIO $ modifyIORef' intentsRef' (tickIntents events)
  liftIO $ readIORef intentsRef'

tickIntents :: [SDL.Event] -> Intents -> Intents
tickIntents events =
  let intentEvents = catMaybes $ eventToIntent <$> events
  in Intents.tick intentEvents

eventToIntent :: SDL.Event -> Maybe IntentEvent
eventToIntent event =
  case event ^. payload of
    (SDL.KeyboardEvent kevent) ->
      keyboardEventToIntentEvent (kevent^.keyMotion) (kevent^.keysym.keycode)
    SDL.QuitEvent -> Just (IntentEvent.Tap Intent.ForceExit)
    _ -> Nothing

keyboardEventToIntentEvent :: SDL.InputMotion -> SDL.Keycode -> Maybe IntentEvent
keyboardEventToIntentEvent SDL.Pressed keycode' = IntentEvent.Tap <$> (keycodeToIntent keycode')
keyboardEventToIntentEvent SDL.Released keycode' = IntentEvent.Release <$> (keycodeToIntent keycode')

keycodeToIntent :: SDL.Keycode -> Maybe Intent
keycodeToIntent SDL.KeycodeReturn = Just Intent.EnterOrConfirm
keycodeToIntent SDL.KeycodeEscape = Just Intent.PauseOrExit
keycodeToIntent SDL.KeycodeW = Just (Intent.Move North)
keycodeToIntent SDL.KeycodeA = Just (Intent.Move West)
keycodeToIntent SDL.KeycodeS = Just (Intent.Move South)
keycodeToIntent SDL.KeycodeD = Just (Intent.Move East)
keycodeToIntent _ = Nothing
