module CrossyToad.Input.MonadInput.SDL.MonadInput
  ( stepInput
  , stepInputState
  , getInputState
  , mkInputEvent
  ) where

import           Control.Lens
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Maybe (catMaybes)
import           Data.Foldable (foldl')
import           Data.IORef (readIORef, modifyIORef')
import qualified SDL.Extended as SDL

import           CrossyToad.Input.InputEvent
import           CrossyToad.Input.InputState
import           CrossyToad.Input.KeyboardState
import           CrossyToad.Input.Key
import           CrossyToad.Input.MonadInput.SDL.Env

stepInput ::
  ( MonadReader r m
  , HasEnv r
  , MonadIO m
  ) => m ()
stepInput = do
  inputStateRef' <- view (env.inputStateRef)
  events <- SDL.pollEvents
  liftIO $ modifyIORef' inputStateRef' (stepInputState events)

getInputState ::
  ( MonadReader r m
  , HasEnv r
  , MonadIO m
  ) => m InputState
getInputState = do
  inputStateRef' <- view (env.inputStateRef)
  liftIO $ readIORef inputStateRef'

stepInputState :: [SDL.Event] -> InputState -> InputState
stepInputState events =
  let inputEvents' = catMaybes $ fmap mkInputEvent events
  in (inputEvents .~ inputEvents')
     . (inputState %~ updateInputState inputEvents')

updateInputState :: [InputEvent] -> InputState -> InputState
updateInputState events inputState' = foldl' (flip updateInputState') inputState' events
  where updateInputState' :: InputEvent -> InputState -> InputState
        updateInputState' (KeyPressed key) = inputState.keyboardState %~ (pressKey key)
        updateInputState' (KeyReleased key) = inputState.keyboardState %~ (releaseKey key)
        updateInputState' QuitEvent = id

mkInputEvent :: SDL.Event -> Maybe InputEvent
mkInputEvent event = case SDL.eventPayload event of
  (SDL.KeyboardEvent (SDL.KeyboardEventData _ motion _ keysym)) -> mkFromKeyboardEvent motion keysym
  (SDL.QuitEvent) -> Just QuitEvent
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
