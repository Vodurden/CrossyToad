module CrossyToad.Title.Title
  ( scene
  ) where

import           Control.Lens
import           Control.Monad (void)
import           Data.Foldable (foldlM)
import           Linear.V2

import           CrossyToad.Input.Intent (Intent(..))
import           CrossyToad.Input.Intents (Intents)
import qualified CrossyToad.Input.Intents as Intents
import           CrossyToad.Physics.Direction (Direction(..))
import qualified CrossyToad.Renderer.Asset.FontAsset as FontAsset
import           CrossyToad.Renderer.Clip (Clip)
import qualified CrossyToad.Renderer.Clip as Clip
import           CrossyToad.Renderer.MonadRenderer (MonadRenderer)
import qualified CrossyToad.Renderer.MonadRenderer as MonadRenderer
import qualified CrossyToad.Renderer.RGBAColour as RGBAColour
import           CrossyToad.Scene.MonadScene (MonadScene)
import qualified CrossyToad.Scene.MonadScene as MonadScene
import           CrossyToad.Scene.Scene (Scene)
import qualified CrossyToad.Scene.Scene as Scene
import qualified CrossyToad.Scene.SceneId as SceneId
import           CrossyToad.Title.MenuOption (MenuOption)
import qualified CrossyToad.Title.MenuOption as MenuOption
import           CrossyToad.Title.TitleState (HasTitleState(..), currentOption)
import qualified CrossyToad.Title.TitleState as TitleState

scene :: (MonadScene m, MonadRenderer m) => Scene m
scene = Scene.mkNoTick TitleState.mk handleInput render

handleInput :: forall m state. (MonadScene m, HasTitleState state) => Intents -> state -> m state
handleInput intents state' =
    foldlM (flip applyIntent) state' (Intents.once intents)
  where
    applyIntent :: Intent -> state -> m state
    applyIntent EnterOrConfirm s = (sceneChange $ s ^. titleState.currentOption) >> pure state'
    applyIntent (Move dir) s = pure $ selectionChange dir s
    applyIntent PauseOrExit s = pure $ s & titleState.currentOption .~ MenuOption.Quit
    applyIntent _ state = pure state

    sceneChange :: MenuOption -> m ()
    sceneChange MenuOption.StartGame = MonadScene.delayPush SceneId.Game
    sceneChange MenuOption.HighScores = MonadScene.delayPush SceneId.Game
    sceneChange MenuOption.Credits = MonadScene.delayPush SceneId.Game
    sceneChange MenuOption.Quit = MonadScene.delayPop

    selectionChange :: Direction -> state -> state
    selectionChange North = titleState %~ TitleState.previousOption
    selectionChange South = titleState %~ TitleState.nextOption
    selectionChange _ = id

render :: (MonadRenderer m, HasTitleState state) => state -> m ()
render state' = do
  MonadRenderer.clearScreen
  renderTitle
  renderMenu (state' ^. currentOption) MenuOption.allOptions
  MonadRenderer.drawScreen

renderTitle :: (MonadRenderer m) => m ()
renderTitle =
    MonadRenderer.drawText
      FontAsset.Title
      Nothing
      Nothing
      (Just titleClip)
      RGBAColour.white
      " CROSSY TOAD "
  where
    titleClip = Clip.shrinkParallel shrinkX shrinkY $ Clip.mkAt titlePos titleSize
    shrinkX = (1280.0 / 10.0)
    shrinkY = (titleSize ^. _y) / 4.0
    titlePos = V2 0 0
    titleSize = V2 1280 (960 / 2.0)

renderMenu :: forall m. (MonadRenderer m) => MenuOption -> [MenuOption] -> m ()
renderMenu selectedOption options =
    void $ foldlM (renderMenuItem) 0 options
  where
    renderMenuItem :: Int -> MenuOption -> m Int
    renderMenuItem offset option = do
      MonadRenderer.drawText
        FontAsset.PressStart
        Nothing
        Nothing
        (Just $ menuItemClip offset)
        (if (option == selectedOption) then RGBAColour.red else RGBAColour.white)
        (MenuOption.text option)

      pure $ offset + 1

    menuItemClip :: Int -> Clip
    menuItemClip offset =
      let
        menuItemWidth = 300.0
        menuItemHeight = 50.0
        menuItemYPadding = 10.0
        menuItemX = (1280.0 / 2.0) - (menuItemWidth / 2.0)
        menuItemY = 500 + (fromIntegral offset * (menuItemHeight + menuItemYPadding))
        menuItemPos = V2 menuItemX menuItemY
      in
        Clip.mkAt menuItemPos (V2 menuItemWidth menuItemHeight)
