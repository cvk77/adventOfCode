module Day09.ParsersSpec (spec) where

import Test.Hspec
import Day09.Parsers

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "parse" $ do
        it "should parse \"Erkelenz to Berlin = 650\"" $ do
            parse "Erkelenz to Berlin = 650" `shouldBe` [(("Erkelenz", "Berlin"), 650), (("Berlin", "Erkelenz"), 650)]
