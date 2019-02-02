module CrossyToad.Scene.Title.Title
  ( handleInput
  , stepIntent
  , render
  ) where

import           Control.Lens
import           Data.Foldable (foldl')

import           CrossyToad.Input.InputState (InputState, HasInputState(..))
import           CrossyToad.Renderer.RenderCommand (RenderCommand(..))
import qualified CrossyToad.Renderer.FontAsset as FontAsset
import qualified CrossyToad.Renderer.RGBAColour as RGBAColour
import           CrossyToad.Scene.Internal (HasScene, scene)
import qualified CrossyToad.Scene.Internal as Scene
import           CrossyToad.Scene.Title.Intent (Intent(..))
import qualified CrossyToad.Scene.Title.Intent as Intent

handleInput :: (HasScene ent) => InputState -> ent -> ent
handleInput input ent =
  let intents = Intent.fromInput (input^.inputEvents)
  in foldl' (flip stepIntent) ent intents

stepIntent :: (HasScene ent) => Intent -> ent -> ent
stepIntent StartGame = scene .~ Scene.Game
stepIntent Quit = scene .~ Scene.Quit

render :: [RenderCommand]
render = [drawTitleText']
  where drawTitleText' =
          DrawText FontAsset.Title
                   Nothing
                   Nothing
                   Nothing
                   RGBAColour.white
                   " CROSSY TOAD "
