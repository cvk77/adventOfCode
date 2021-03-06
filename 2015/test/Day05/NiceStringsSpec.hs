module Day05.NiceStringsSpec (spec) where

import Test.Hspec
import Day05.NiceStrings (isNice, isNice')

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "isNice" $ do
        it "works for ugknbfddgicrmopn" $ do
            isNice "ugknbfddgicrmopn" `shouldBe` True
        it "works for aaa" $ do
            isNice "aaa" `shouldBe` True
        it "works for jchzalrnumimnmhp" $ do
            isNice "jchzalrnumimnmhp" `shouldBe` False
        it "works for haegwjzuvuyypxyu" $ do
            isNice "haegwjzuvuyypxyu" `shouldBe` False
        it "works for dvszwmarrgswjxmb" $ do
            isNice "dvszwmarrgswjxmb" `shouldBe` False

    describe "isNice'" $ do
        it "works for qjhvhtzxzqqjkmpb" $ do
            isNice' "qjhvhtzxzqqjkmpb" `shouldBe` True
        it "works for xxyxx" $ do
            isNice' "xxyxx" `shouldBe` True
        it "works for uurcxstgmygtbstg" $ do
            isNice' "uurcxstgmygtbstg" `shouldBe` False
        it "works for ieodomkazucvgmuy" $ do
            isNice' "ieodomkazucvgmuy" `shouldBe` False
