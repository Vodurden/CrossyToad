{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Game.Intent
  ( Intent(..)
  , AsIntent(..)
  , fromInputState
  ) where

import           Control.Lens
import           Control.Monad (mfilter)
import           Data.Maybe (catMaybes)

import           CrossyToad.Input.InputEvent
import           CrossyToad.Input.InputState
import           CrossyToad.Input.Key (Key)
import qualified CrossyToad.Input.Key as Key
import           CrossyToad.Input.KeyboardState
import           CrossyToad.Physics.Physics

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
    [ ifDown Key.W (Move North)
    , ifDown Key.A (Move West)
    , ifDown Key.S (Move South)
    , ifDown Key.D (Move East)
    ]
  where
    ifDown :: Key -> Intent -> Maybe Intent
    ifDown key intent = mfilter (const $ keyDown ks key) (Just $ intent)

fromEvents :: [InputEvent] -> [Intent]
fromEvents events = catMaybes $ fmap fromInput' events
  where
    fromInput' :: InputEvent -> Maybe Intent
    fromInput' (KeyPressed Key.Escape) = Just Exit
    fromInput' _ = Nothing
