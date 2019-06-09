{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Title.TitleState
  ( TitleState
  , HasTitleState(..)
  , mk
  , nextOption
  , previousOption
  ) where

import Control.Lens
import Data.Bounded.Extended (succWrap, predWrap)

import CrossyToad.Title.MenuOption

data TitleState = TitleState
  { _currentOption :: MenuOption
  } deriving (Eq, Show)

makeClassy ''TitleState

mk :: TitleState
mk = TitleState
  { _currentOption = StartGame
  }

nextOption :: TitleState -> TitleState
nextOption = currentOption %~ succWrap

previousOption :: TitleState -> TitleState
previousOption = currentOption %~ predWrap
