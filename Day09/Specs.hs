module Day09.Specs where

import Test.Hspec
import Day09.Parsers
import Day09.Distance (distances, routes, distance)
import qualified Data.Map as Map

{-# ANN module "HLint: ignore Redundant do" #-}

testData = [ "London to Dublin = 464"
           , "London to Belfast = 518"
           , "Dublin to Belfast = 141"
           ]

main :: IO ()
main = hspec $ do
    describe "parse" $ do
        it "should parse \"Erkelenz to Berlin = 650\"" $ do
            parse "Erkelenz to Berlin = 650" `shouldBe` [(("Erkelenz", "Berlin"), 650), (("Berlin", "Erkelenz"), 650)]

    describe "distances" $ do
        let ds = distances testData
        let rs = routes ds
        it "should correctly parse the inputs" $ do
            ds `shouldBe` Map.fromList  [
                (("Belfast","Dublin"),141),(("Belfast","London"),518),
                (("Dublin","Belfast"),141), (("Dublin","London"),464),
                (("London","Belfast"),518),(("London","Dublin"),464)]
        it "should find the possible routes" $ do
            rs `shouldBe` [
                ["Belfast","Dublin","London"], ["Dublin","Belfast","London"],
                ["London","Dublin","Belfast"], ["Dublin","London","Belfast"],
                ["London","Belfast","Dublin"], ["Belfast","London","Dublin"]]
        it "should calculate the correct distance for Belfast -> Dublin -> London = 605" $ do
            distance ds ["Belfast","Dublin","London"] `shouldBe` 605
        it "should calculate the correct distance for London -> Dublin -> Belfast = 605" $ do
            distance ds ["London","Dublin","Belfast"] `shouldBe` 605
        it "should calculate the correct distance for Dublin -> Belfast -> London = 659" $ do
            distance ds ["Dublin","Belfast","London"] `shouldBe` 659
        it "should calculate the correct distance for London -> Belfast -> Dublin = 659" $ do
            distance ds ["London","Belfast","Dublin"] `shouldBe` 659
        it "should calculate the correct distance for Belfast -> London -> Dublin = 982" $ do
            distance ds ["Belfast","London","Dublin"] `shouldBe` 982
        it "should calculate the correct distance for Dublin -> London -> Belfast = 982" $ do
            distance ds ["Dublin","London","Belfast"] `shouldBe` 982
