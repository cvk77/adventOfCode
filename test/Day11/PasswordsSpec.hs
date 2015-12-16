module Day11.PasswordsSpec where

import Test.Hspec
import Day11.Passwords (nextCandidate, hasStraight, noForbidden, hasPair, nextPassword)

spec :: Spec
spec = do
    describe "nextCandidate" $ do
        it "should find b after a" $ do
            nextCandidate "a" `shouldBe` "b"
        it "should find aa after z" $ do
            nextCandidate "z" `shouldBe` "aa"
        it "should find ba after az" $ do
            nextCandidate "az" `shouldBe` "ba"
        it "should find aaa after zz" $ do
            nextCandidate "zz" `shouldBe` "aaa"

    describe "hasStraight" $ do
        it "should accept hij" $ do
            hasStraight "hij" `shouldBe` True
        it "should fail h" $ do
            hasStraight "h" `shouldBe` False
        it "should accept hijklmmn" $ do
            hasStraight "hijklmmn" `shouldBe` True
        it "should fail abbceffg" $ do
            hasStraight "abbceffg" `shouldBe` False

    describe "noForbidden" $ do
        it "should fail hijklmmn" $ do
            noForbidden "hijklmmn" `shouldBe` False
        it "should accept abbceffg " $ do
            noForbidden "abbceffg" `shouldBe` True

    describe "hasPair" $ do
        it "should accept abbceffg" $ do
            hasPair "abbceffg" `shouldBe` True
        it "should fail abbcegjk" $ do
            hasPair "abbcegjk" `shouldBe` False

    describe "nextPassword" $ do
        it "should find abcdffaa after abcdefgh" $ do
            nextPassword "abcdefgh" `shouldBe` "abcdffaa"
        it "should find ghjaabcc after ghijklmn" $ do
            nextPassword "ghijklmn" `shouldBe` "ghjaabcc"
