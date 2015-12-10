module Day2.Specs where

import Test.Hspec
import Day2.GiftWrap (paperNeeded, ribbonNeeded, mapOverData, sumMaybe)

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "paperNeeded" $ do
        it "should return 58 for 2x3x4" $ do
            paperNeeded 2 3 4 `shouldBe` 58
        it "should return 43 for 1x1x10" $ do
            paperNeeded 1 1 10 `shouldBe` 43

    describe "mapOverData" $ do
        it "should correctly map addition over a valid list" $ do
            mapOverData (\a b c -> a+b+c) ["2x3x4", "1x2x3"] `shouldBe` [Just 9, Just 6]
        it "should return Nothing for invalid input" $ do
            mapOverData (\a b c -> a+b+c) ["2x3x4", "foo"] `shouldBe` [Just 9, Nothing]

    describe "sumMaybe" $ do
        it "should sum the Justs in a list" $ do
            sumMaybe [Just 2, Just 3, Just 0] `shouldBe` (Just 5)
        it "should return Nothing if there is a Nothing" $ do
            sumMaybe [Just 2, Nothing, Just 0] `shouldBe` Nothing

    describe "ribbonNeeded" $ do
        it "should return 34 for 2x3x4" $ do
            ribbonNeeded 2 3 4 `shouldBe` 34
        it "should return 14 for 1x1x10" $ do
            ribbonNeeded 1 1 10 `shouldBe` 14
