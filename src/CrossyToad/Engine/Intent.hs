{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Engine.Intent where

import Control.Lens
import Data.Maybe (catMaybes)

import CrossyToad.Engine.KeyState
import CrossyToad.Engine.InputState

data Intent = Quit
            | StartGame

makeClassyPrisms ''Intent

fromInput :: InputState -> [Intent]
fromInput input = catMaybes $
    [ onInput Pressed enter StartGame
    ] <*> repeat input

onInputPressed :: Lens' s KeyState -> Intent -> s -> Maybe Intent
onInputPressed = onInput Pressed

onInput :: KeyState -> Lens' s KeyState -> Intent -> s -> Maybe Intent
onInput expectedKeyState keyStateL intent input | input^.keyStateL == expectedKeyState = Just intent
                                                | otherwise = Nothing
