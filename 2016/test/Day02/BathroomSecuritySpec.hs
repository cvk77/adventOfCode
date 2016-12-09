module Day02.BathroomSecuritySpec where

import Prelude hiding (Left, Right)
import Test.Hspec
import Day02.BathroomSecurity 

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "puzzle1" $ do
        it "should return \"1985\" for [\"ULL\", \"RRDDD\", \"LURDL\", \"UUUUD\"] starting at '5'" $ do
            puzzle1 ["ULL", "RRDDD", "LURDL", "UUUUD"] '5' `shouldMatchList` "1985"
    describe "puzzle2" $ do
        it "should return \"5DB3\" for [\"ULL\", \"RRDDD\", \"LURDL\", \"UUUUD\"] starting at '5'" $ do
            puzzle2 ["ULL", "RRDDD", "LURDL", "UUUUD"] '5' `shouldMatchList` "5DB3"
