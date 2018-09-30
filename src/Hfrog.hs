{-# LANGUAGE OverloadedStrings #-}
module Hfrog where

import Control.Monad (unless)

import           SDL (($=))
import qualified SDL as SDL

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Hfrog" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  mainLoop renderer

mainLoop :: SDL.Renderer -> IO ()
mainLoop renderer = do
  events <- SDL.pollEvents
  let eventIsQPress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  SDL.rendererDrawColor renderer $= SDL.V4 0 0 255 255
  SDL.clear renderer
  SDL.present renderer
  unless qPressed (mainLoop renderer)

message :: String
message = "Hello, Hfrog!"
