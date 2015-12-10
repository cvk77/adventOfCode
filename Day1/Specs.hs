module Day1.Specs where

import Test.Hspec
import Day1.Parantheses (delta, steps)

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "delta" $ do
        it "should return 0 for (())" $ do
            delta "(())" `shouldBe` 0
        it "should return 0 for ()()" $ do
            delta "()()" `shouldBe` 0
        it "should return 3 for (((" $ do
            delta "(((" `shouldBe` 3
        it "should return 3 for (()(()(" $ do
            delta "(()(()(" `shouldBe` 3
        it "should return 3 for ))(((((" $ do
            delta "))(((((" `shouldBe` 3
        it "should return -1 for ())" $ do
            delta "())" `shouldBe` -1
        it "should return -1 for ))(" $ do
            delta "))(" `shouldBe` -1
        it "should return -3 for )))" $ do
            delta ")))" `shouldBe` -3
        it "should return -3 for )())())" $ do
            delta ")())())" `shouldBe` -3

    describe "steps" $ do
        it "should return the correct steps for (())()" $ do
            steps "(())()" `shouldBe` [0,1,2,1,0,1,0]
