module Day01.EasterBunnyHeadquartersSpec where

import Test.Hspec
import Day01.EasterBunnyHeadquarters (puzzle1, puzzle2)

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "puzzle1" $ do
        it "should return 5 for R2, L3" $ do
            puzzle1 "R2, L3" `shouldBe` 5
        it "should return 2 for R2, R2, R2" $ do
            puzzle1 "R2, R2, R2" `shouldBe` 2
        it "should return 12 for R5, L5, R5, R3" $ do
            puzzle1 "R5, L5, R5, R3" `shouldBe` 12
    describe "puzzle2" $ do
        it "should return 4 for R8, R4, R4, R8" $ do
            puzzle2 "R8, R4, R4, R8" `shouldBe` Just 4