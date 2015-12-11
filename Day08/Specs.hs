module Day08.Specs where

import Test.Hspec
import Day08.DigitalList (codeLength, memoryLength, encodedLength)

{-# ANN module "HLint: ignore Redundant do" #-}

testStrings = [ "\"\""
              , "\"abc\""
              , "\"aaa\\\"aaa\""
              , "\"\\x27\""
              ]

main :: IO ()
main = hspec $ do
    describe "codeLength" $ do
        it "should calculate the code length of \"\" correctly" $ do
            codeLength "\"\"" `shouldBe` 2
        it "should calculate the code length of \"abc\" correctly" $ do
            codeLength "\"abc\"" `shouldBe` 5
        it "should calculate the code length of \"abc\" correctly" $ do
            codeLength "\"abc\"" `shouldBe` 5
        it "should calculate the code length of \"aaa\\\"aaa\" correctly" $ do
            codeLength "\"aaa\\\"aaa\"" `shouldBe` 10
        it "should calculate the code length of \"\\x27\" correctly" $ do
            codeLength "\"\\x27\"" `shouldBe` 6
    describe "memoryLength" $ do
        it "should calculate the memory length of \"\" correctly" $ do
            memoryLength "\"\"" `shouldBe` 0
        it "should calculate the memory length of \"abc\" correctly" $ do
            memoryLength "\"abc\"" `shouldBe` 3
        it "should calculate the memory length of \"aaa\\\"aaa\" correctly" $ do
            memoryLength "\"aaa\\\"aaa\"" `shouldBe` 7
        it "should calculate the memory length of \"aaa\\\"aaa\" correctly" $ do
            memoryLength "\"aaa\\\"aaa\"" `shouldBe` 7
        it "should calculate the memory length of \"\\x27\" correctly" $ do
            memoryLength "\"\\x27\"" `shouldBe` 1
    describe "encodedLength" $ do
        it "should calculate the encoded length of \"\" correctly" $ do
            encodedLength "\"\"" `shouldBe` 6
        it "should calculate the encoded length of \"abc\" correctly" $ do
            encodedLength "\"abc\"" `shouldBe` 9
        it "should calculate the encoded length of \"aaa\\\"aaa\" correctly" $ do
            encodedLength "\"aaa\\\"aaa\"" `shouldBe` 16
        it "should calculate the encoded length of \"\\x27\" correctly" $ do
            encodedLength "\"\\x27\"" `shouldBe` 11
        it "should calculate the encoded length of \"\\x27\" correctly" $ do
            encodedLength "\"\\x27\"" `shouldBe` 11
