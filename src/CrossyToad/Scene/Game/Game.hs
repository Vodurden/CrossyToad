module CrossyToad.Scene.Game.Game
  ( initialize
  , stepGame
  , stepIntent
  , module CrossyToad.Scene.Game.GameState
  ) where

import           Control.Lens.Extended
import           Control.Monad.State.Extended (StateT, State, hoistState)
import           Data.Foldable (traverse_)
import           Linear.V2

import           CrossyToad.Physics.Physics (Direction(..))
import           CrossyToad.Input.Input (Input(..))
import           CrossyToad.Renderer.Renderer (Renderer(..))
import           CrossyToad.Time.Time (Time(..))
import           CrossyToad.Scene.Internal (HasScene, scene)
import qualified CrossyToad.Scene.Internal as Scene
import           CrossyToad.Scene.Game.Intent (Intent(..))
import qualified CrossyToad.Scene.Game.Intent as Intent
import           CrossyToad.Scene.Game.GameState
import           CrossyToad.Scene.Game.Toad
import qualified CrossyToad.Scene.Game.Toad as Toad
import           CrossyToad.Scene.Game.Car (HasCars(..))
import qualified CrossyToad.Scene.Game.Car as Car
import qualified CrossyToad.Scene.Game.Collision as Collision
import qualified CrossyToad.Scene.Game.SpawnPoint as SpawnPoint
import           CrossyToad.Scene.Game.SpawnPoint (HasSpawnPoints(..))

initialize :: (HasGameState s) => State s ()
initialize = do
  gameState.toad .= Toad.mk (V2 (7*64) (13*64))
  gameState.cars .= []
  gameState.spawnPoints .=
    [ SpawnPoint.mk (V2 0 1*64) East 1 1
    , SpawnPoint.mk (V2 (14*64) (3*64)) West 1 1
    ]


-- | Steps the state of the game.
-- |
-- | Should be executed once per frame when this scene is active.
stepGame ::
  ( HasGameState s
  , HasScene s
  , Input m
  , Renderer m
  , Time m
  ) => StateT s m ()
stepGame = do
  inputState' <- getInputState
  hoistState $ traverse_ stepIntent (Intent.fromInputState inputState')

  zoom gameState $ do
    Toad.step
    SpawnPoint.stepAll
    Car.stepAll
    Collision.step
    renderGame

-- | Applies the intent of the user to the scene
stepIntent :: (HasGameState s , HasScene s) => Intent -> State s ()
stepIntent (Move dir) = gameState.toad %= Toad.jump dir
stepIntent Exit = scene .= Scene.Title

-- | Draws the Scene on the screen
renderGame :: (HasToad s, HasCars s, Renderer m) => StateT s m ()
renderGame = do
  cars' <- use cars
  traverse_ Car.render cars'

  toad' <- use toad
  Toad.render toad'
