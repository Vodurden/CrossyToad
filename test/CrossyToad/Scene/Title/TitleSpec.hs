module CrossyToad.Scene.Title.TitleSpec where

-- import           Control.Lens
-- import           Test.Tasty.Hspec

-- import           CrossyToad.Renderer.RenderCommand
-- import qualified CrossyToad.Scene.Scene as Scene
-- import qualified CrossyToad.Scene.Title.Intent as Intent
-- import           CrossyToad.Scene.Title.Title

-- spec_Scene_Title :: Spec
-- spec_Scene_Title = do
--   describe "stepIntent" $ do
--     it "should start the game when the intent is StartGame" $
--       stepIntent Intent.StartGame Scene.Title `shouldBe` Scene.Game

--     it "should quit the game when the intent is Quit" $
--       stepIntent Intent.Quit Scene.Title `shouldBe` Scene.Quit

--   describe "render" $ do
--     it "should draw ' CROSSY TOAD ' on the screen" $ do
--       let texts = (^? _DrawText . _6) <$> render
--       texts `shouldContain` [(Just " CROSSY TOAD ")]
