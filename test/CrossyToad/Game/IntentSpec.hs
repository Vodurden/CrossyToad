module CrossyToad.Game.IntentSpec where

-- import Test.Tasty.Hspec

-- import Control.Lens

-- import           CrossyToad.Input.InputState
-- import           CrossyToad.Input.InputEvent
-- import           CrossyToad.Input.KeyboardState
-- import           CrossyToad.Input.Key (Key)
-- import qualified CrossyToad.Input.Key as Key
-- import CrossyToad.Physics.Physics
-- import CrossyToad.Game.Intent

-- mkInputPressed :: Key -> InputState
-- mkInputPressed key = initialInputState
--   & (keyboardState %~ pressKey key)
--   . (inputEvents .~ [KeyPressed key])

-- mkInputHeld :: Key -> InputState
-- mkInputHeld = mkInputPressed

-- spec_Scene_Game_Intent :: Spec
-- spec_Scene_Game_Intent =
--   describe "fromInputState" $ do
--     it "should Exit when escape is pressed" $ do
--       fromInputState (mkInputPressed Key.Escape) `shouldBe` [Exit]

--     it "should Move North if W is held" $ do
--       fromInputState (mkInputHeld Key.W) `shouldBe` [Move North]

--     it "should Move West if A is held" $ do
--       fromInputState (mkInputHeld Key.A) `shouldBe` [Move West]

--     it "should Move South if S is held" $ do
--       fromInputState (mkInputHeld Key.S) `shouldBe` [Move South]

--     it "should Move East if D is held" $ do
--       fromInputState (mkInputHeld Key.D) `shouldBe` [Move East]
