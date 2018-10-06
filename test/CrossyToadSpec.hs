module CrossyToadSpec  where

import Test.Tasty.Hspec

import CrossyToad

spec_hfrog :: Spec
spec_hfrog = describe "message" $
  it "is 'Hello, Crossy Toad!'" $
    message `shouldBe` "Hello, Crossy Toad!"
