module CrossyFrogSpec  where

import Test.Tasty.Hspec

import CrossyFrog

spec_hfrog :: Spec
spec_hfrog = describe "message" $
  it "is 'Hello, Crossy Frog!'" $
    message `shouldBe` "Hello, Crossy Frog!"
