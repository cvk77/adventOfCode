{-# LANGUAGE OverloadedStrings #-}

module Day04.AdventCoinsSpec (spec) where

import Test.Hspec
import Day04.AdventCoins (mine)

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = do
    describe "adventCoins" $ do
        it "If your secret key is abcdef, the answer is 609043" $ do
            mine 5 "abcdef" `shouldBe` Just 609043
        it "If your secret key is pqrstuv, the answer is 1048970" $ do
            mine 5 "pqrstuv" `shouldBe` Just 1048970
