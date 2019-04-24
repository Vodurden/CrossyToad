{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Renderer.SpriteSpec where

-- import           Control.Lens
-- import           Linear.V2

-- import           Test.Tasty.Hspec

-- import           CrossyToad.Renderer.Asset.ImageAsset (HasImageAsset(..))
-- import qualified CrossyToad.Renderer.Asset.ImageAsset as ImageAsset
-- import           CrossyToad.Renderer.RenderCommand
-- import           CrossyToad.Geometry.Position
-- import           CrossyToad.Geometry.Size
-- import           CrossyToad.Physics.Direction
-- import           CrossyToad.Renderer.Sprite as Sprite

-- data Ent = Ent
--   { __position :: !Position
--   , __direction :: !Direction
--   , __sprite :: !Sprite
--   }

-- makeClassy ''Ent

-- instance HasPosition Ent where position = _position
-- instance HasDirection Ent where direction = _direction
-- instance HasSprite Ent where sprite = _sprite

-- mkEnt :: Ent
-- mkEnt = Ent
--   { __position = (V2 0 0)
--   , __direction = North
--   , __sprite = Sprite
--     { __imageAsset = ImageAsset.Toad
--     , __size = V2 64 64
--     }
--   }

-- spec_Sprite_SpriteSpec :: Spec
-- spec_Sprite_SpriteSpec = do
--   describe "render" $ do
--     it "should render the given image asset" $ do
--       let ent' = mkEnt & sprite . imageAsset .~ ImageAsset.Toad
--       let asset = (render ent') ^? _Draw . _1
--       asset `shouldBe` Just ImageAsset.Toad

--     it "should not rotate when facing north" $ do
--       let ent' = mkEnt & direction .~ North
--       let rotation = (render ent') ^? _Draw . _2 . _Just
--       rotation `shouldBe` Nothing

--     it "should rotate 90 degrees when facing east" $ do
--       let ent' = mkEnt & direction .~ East
--       let rotation = (render ent') ^? _Draw . _2 . _Just
--       rotation `shouldBe` Just 90

--     it "should rotate 180 degrees when facing south" $ do
--       let ent' = mkEnt & direction .~ South
--       let rotation = (render ent') ^? _Draw . _2 . _Just
--       rotation `shouldBe` (Just 180)

--     it "should rotate 270 degrees when facing west" $ do
--       let ent' = mkEnt & direction .~ West
--       let rotation = (render ent') ^? _Draw . _2 . _Just
--       rotation `shouldBe` (Just 270)

--     it "should render at the entities position" $ do
--       let ent' = mkEnt & position .~ V2 25 25
--       let pos = (render ent') ^? _Draw . _4 . _Just . position
--       pos `shouldBe` Just (V2 25 25)

--     it "should not specify a texture clip" $ do
--       let ent' = mkEnt
--       let textureClip = (render ent') ^? _Draw . _3 . _Just
--       textureClip `shouldBe` Nothing

--     it "should clip the image to the dimensions" $ do
--       let ent' = mkEnt & sprite . size .~ V2 64 64
--       let dimensions' = (render ent') ^? _Draw . _4 . _Just . size
--       dimensions' `shouldBe` Just (V2 64 64)
