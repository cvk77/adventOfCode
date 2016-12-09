module Day02.BathroomSecuritySpec where

import Prelude hiding (Left, Right)
import Test.Hspec
import Day02.BathroomSecurity 

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
    describe "puzzle1" $ do
        it "should return [1,9,8,5] for [\"ULL\", \"RRDDD\", \"LURDL\", \"UUUUD\"] starting at 5" $ do
            puzzle1 ["ULL", "RRDDD", "LURDL", "UUUUD"] 5 `shouldMatchList` [1,9,8,5]
