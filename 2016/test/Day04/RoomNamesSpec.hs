module Day04.RoomNamesSpec where

import Test.Hspec
import Day04.RoomNames 

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "parseRoom" $ do
        it "should parse 'aaaaa-bbb-z-y-x-123[abxyz]'" $ do
            parseRoom "aaaaa-bbb-z-y-x-123[abxyz]" `shouldBe` Just (Room "aaaaa-bbb-z-y-x" 123 "abxyz")
        it "should parse 'a-b-c-d-e-f-g-h-987[abcde]'" $ do
            parseRoom "a-b-c-d-e-f-g-h-987[abcde]" `shouldBe` Just (Room "a-b-c-d-e-f-g-h" 987 "abcde")
        it "should parse 'not-a-real-room-404[oarel]'" $ do
            parseRoom "not-a-real-room-404[oarel]" `shouldBe` Just (Room "not-a-real-room" 404 "oarel")
        it "should parse 'totally-real-room-200[decoy]'" $ do
            parseRoom "totally-real-room-200[decoy]" `shouldBe` Just (Room "totally-real-room" 200 "decoy")
    
    describe "checkSectorId" $ do
        it "should accept 'aaaaa-bbb-z-y-x-123[abxyz]'" $ do
            checkSectorId (Room "aaaaa-bbb-z-y-x" 123 "abxyz") `shouldBe` Just (Room "aaaaa-bbb-z-y-x" 123 "abxyz")
        it "should accept 'a-b-c-d-e-f-g-h-987[abcde]'" $ do
            checkSectorId (Room "a-b-c-d-e-f-g-h" 987 "abcde") `shouldBe` Just (Room "a-b-c-d-e-f-g-h" 987 "abcde")
        it "should accept 'not-a-real-room-404[oarel]'" $ do
            checkSectorId (Room "not-a-real-room" 404 "oarel") `shouldBe` Just (Room "not-a-real-room" 404 "oarel")
        it "should reject 'totally-real-room-200[decoy]'" $ do
            checkSectorId (Room "totally-real-room" 200 "decoy") `shouldBe` Nothing

    describe "decodeName" $ do
        it "should decode 'qzmt-zixmtkozy-ivhz-343' to 'very encrypted name'" $ do
            decodeName (Room "qzmt-zixmtkozy-ivhz" 343 "") `shouldBe` Just (Room "very encrypted name" 343 "")

