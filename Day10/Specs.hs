module Day10.Specs where

import Test.Hspec
import Day10.LookAndSay (lookAndSay, nthLength)

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "lookAndSay" $ do
        it "creates the first 10 numbers correctly" $ do
            take 10 (iterate lookAndSay "1") `shouldBe` ["1","11","21","1211","111221","312211","13112221","1113213211","31131211131221","13211311123113112211"]
    describe "nthLength" $ do
        it "correctly finds the length of the 1. test number" $ do
            nthLength 1 `shouldBe` 10
        it "correctly finds the length of the 2. test number" $ do
            nthLength 2 `shouldBe` 12
        it "correctly finds the length of the 3. test number" $ do
            nthLength 3 `shouldBe` 18
        it "correctly finds the length of the 4. test number" $ do
            nthLength 4 `shouldBe` 22
        it "correctly finds the length of the 5. test number" $ do
            nthLength 5 `shouldBe` 28
        it "correctly finds the length of the 6. test number" $ do
            nthLength 6 `shouldBe` 38
        it "correctly finds the length of the 7. test number" $ do
            nthLength 7 `shouldBe` 48
        it "correctly finds the length of the 8. test number" $ do
            nthLength 8 `shouldBe` 60
        it "correctly finds the length of the 9. test number" $ do
            nthLength 9 `shouldBe` 76
        it "correctly finds the length of the 10. test number" $ do
            nthLength 10 `shouldBe` 100
