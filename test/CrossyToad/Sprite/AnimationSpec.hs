{-# LANGUAGE TemplateHaskell #-}

module CrossyToad.Sprite.AnimationSpec where

-- import           Control.Lens
-- import           Linear.V2

-- import           Test.Tasty.Hspec

-- import qualified CrossyToad.Effect.Renderer.ImageAsset as ImageAsset
-- import           CrossyToad.Effect.Renderer.PixelClip (PixelClip(PixelClip))
-- import           CrossyToad.Effect.Renderer.RenderCommand
-- import           CrossyToad.Physics.Direction
-- import           CrossyToad.Physics.Position
-- import           CrossyToad.Sprite.Animation as Animation
-- import           CrossyToad.Sprite.Sprite (Sprite(..), HasSprite(..))
-- import qualified CrossyToad.Sprite.Sprite as Sprite

-- data Ent = Ent
--   { __position :: !Position
--   , __direction :: !Direction
--   , __sprite :: !Sprite
--   , __animation :: !(Animation Int)
--   }

-- makeClassy ''Ent

-- instance HasPosition Ent where position = _position
-- instance HasDirection Ent where direction = _direction
-- instance HasSprite Ent where sprite = _sprite
-- instance HasAnimation Ent where animation = _animation

-- mkEnt :: Ent
-- mkEnt = Ent
--   { __position = (V2 0 0)
--   , __direction = North
--   , __sprite = Sprite
--     { __imageAsset = ImageAsset.Toad
--     , __dimensions = V2 64 64
--     }
--   , __animation = Animation
--     { _textureClip = PixelClip (V2 0 0) (V2 64 64)
--     }
--   }

-- spec_Sprite_AnimationSpec :: Spec
-- spec_Sprite_AnimationSpec = do
--   describe "render" $ do
--     it "should apply the texture clip to the sprite" $ do
--       let clip = PixelClip (V2 0 0) (V2 64 64)
--       let ent' = mkEnt & animation . textureClip .~ clip
--       let resultClip = (render ent') ^? _Draw . _3 . _Just
--       resultClip `shouldBe` (Just clip)

--     it "should not modify anything other then the texture clip" $ do
--       let ent' = mkEnt
--       let spriteDraw = Sprite.render ent'
--       let animationDraw = render ent'

--       let spriteDrawNoTex = spriteDraw & _Draw . _3 .~ Nothing
--       let animationDrawNoTex = animationDraw & _Draw . _3 .~ Nothing

--       spriteDrawNoTex `shouldBe` animationDrawNoTex
