module HfrogSpec (spec_hfrog) where

import Test.Tasty.Hspec

import Hfrog

spec_hfrog :: Spec
spec_hfrog = describe "message" $
  it "is 'Hello, Hfrog!'" $
    message `shouldBe` "Hello, Hfrog!"
