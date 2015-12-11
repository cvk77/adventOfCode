module Day9.Specs where

import Test.Hspec
import Day9.Parsers

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "parse" $ do
        it "should parse \"Erkelenz to Berlin = 650\"" $ do
            parse "Erkelenz to Berlin = 650" `shouldBe` (("Erkelenz", "Berlin"), 650)