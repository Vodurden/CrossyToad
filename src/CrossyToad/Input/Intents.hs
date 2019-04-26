{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Input.Intents
  ( Intents
  , initialize
  , tick
  , all
  , once
  , continuous
  ) where

import           Prelude hiding (all)
import           Data.Map.Strict.Extended (Map)
import qualified Data.Map.Strict.Extended as Map
import           Data.Foldable (foldl')

import           CrossyToad.Input.Intent (Intent)
import           CrossyToad.Input.IntentEvent (IntentEvent(..))

type Intents = Map Intent IntentState

data IntentState
  = Once
  | Continually
  deriving (Eq, Show, Ord)

initialize :: Intents
initialize = Map.empty

tick :: [IntentEvent] -> Intents -> Intents
tick events intents' = foldl' (flip applyEvent) (tickExisting intents') events
  where
    tickExisting :: Intents -> Intents
    tickExisting = Map.map (const Continually)

    applyEvent :: IntentEvent -> Intents -> Intents
    applyEvent (Tap intent') = Map.create intent' Once
    applyEvent (Release intent') = Map.delete intent'

all :: Intents -> [Intent]
all = matchedEvents (const True)

once :: Intents -> [Intent]
once = matchedEvents ((==) Once)

continuous :: Intents -> [Intent]
continuous = matchedEvents ((==) Continually)

matchedEvents :: (IntentState -> Bool) -> Intents -> [Intent]
matchedEvents f intents' = fst <$> (Map.toList $ Map.filter f intents')
