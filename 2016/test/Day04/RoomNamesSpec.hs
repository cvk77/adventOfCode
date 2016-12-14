module Day04.RoomNamesSpec where

import Test.Hspec
import Day04.RoomNames 

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "puzzle1" $ do
        it "should accept 'aaaaa-bbb-z-y-x-123[abxyz]'" $ do
            puzzle1 "aaaaa-bbb-z-y-x-123[abxyz]" `shouldBe` Just 123
        it "should accept 'a-b-c-d-e-f-g-h-987[abcde]'" $ do
            puzzle1 "a-b-c-d-e-f-g-h-987[abcde]" `shouldBe` Just 987
        it "should accept 'not-a-real-room-404[oarel]'" $ do
            puzzle1 "not-a-real-room-404[oarel]" `shouldBe` Just 404
        it "should accept 'totally-real-room-200[decoy]'" $ do
            puzzle1 "totally-real-room-200[decoy]" `shouldBe` Nothing
    
    describe "puzzle2" $ do
        it "should decode 'qzmt-zixmtkozy-ivhz-343[zimth]' to 'very encrypted name'" $ do
            puzzle2 "qzmt-zixmtkozy-ivhz-343[zimth]" `shouldBe` Just (Room "very encrypted name" 343 "zimth")

