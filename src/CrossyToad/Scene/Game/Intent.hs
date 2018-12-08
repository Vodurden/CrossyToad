{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Scene.Game.Intent
  ( Intent(..)
  , AsIntent(..)
  , fromInputState
  ) where

import Control.Lens
import Control.Monad (mfilter)
import Data.Maybe (catMaybes)

import CrossyToad.Effect.Input.Input
import CrossyToad.Geometry.Geometry

data Intent = Move Direction
            | Exit
            deriving (Show, Eq)

makeClassyPrisms ''Intent

fromInputState :: InputState -> [Intent]
fromInputState inputState' =
  (fromEvents $ inputState' ^. inputEvents)
  ++ (fromKeyboardState $ inputState' ^. keyboardState)

fromKeyboardState :: KeyboardState -> [Intent]
fromKeyboardState ks = catMaybes $
    [ ifDown W (Move North)
    , ifDown A (Move West)
    , ifDown S (Move South)
    , ifDown D (Move East)
    ]
  where
    ifDown :: Key -> Intent -> Maybe Intent
    ifDown key intent = mfilter (const $ keyDown ks key) (Just $ intent)

fromEvents :: [InputEvent] -> [Intent]
fromEvents events = catMaybes $ fmap fromInput' events
  where
    fromInput' :: InputEvent -> Maybe Intent
    fromInput' (KeyPressed Escape) = Just Exit
    fromInput' _ = Nothing
