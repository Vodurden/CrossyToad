module CrossyToad.Game.Stage.StageSpec where

-- import           Test.Tasty.Hspec

-- spec_Game_Stage_StageSpec :: Spec
-- spec_Game_Stage_StageSpec =
--   describe "load" $ do
--     it "should load stages from valid text" $ do
--       stageText = unlines
--         [ "|   H    H    H    H   |Swamp"
--         , "| TTT TTT DDD TTT TTT  |River East 0.75"
--         , "|   LLLL LLLL LLLL LLLL|River East 0.75"
--         , "| LL  LL  LL  LL  LL   |River West 1"
--         , "|TT TT TT TT DD TT TT  |River West 1"
--         , "|                      |Grass East 0"
--         , "|             TR    TR |Road  East 0.5"
--         , "| TR      TR           |Road  East 0.5"
--         , "|F         F           |Road  East 0.5"
--         , "|C   C   C   C   C     |Road  East 0.5"
--         , "|  C     C     C    C  |Road  East 0.5"
--         , "|                      |Grass East 0"
--         ]

--       stage = Stage.load stageText

--       expectedRows =
--         [ StageRow [Empty, Empty, Empty, ToadHome, Empty, Empty, Empty, ]
--         ]

--       stage `shouldBe` Stage { _rows = expectedRows }
