import Test.Hspec
import Delivery (housesVisited, housesVisited')

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "housesVisited" $ do
        it "should return 2 for >" $ do
            housesVisited ">" `shouldBe` 2
        it "should return 4 for ^>v<" $ do
            housesVisited "^>v<" `shouldBe` 4
        it "should return 2 for ^v^v^v^v^v" $ do
            housesVisited "^v^v^v^v^v" `shouldBe` 2

    describe "housesVisited" $ do
        it "^v delivers presents to 3 houses" $ do
            housesVisited' "^v" `shouldBe` 3
        it "^>v< delivers presents to 3 houses" $ do
            housesVisited' "^>v<" `shouldBe` 3
        it "^v^v^v^v^v delivers presents to 11 houses" $ do
            housesVisited' "^v^v^v^v^v" `shouldBe` 11
        it "^>>^^> delivers presents to 6 houses" $ do
            housesVisited' "^>>^^>" `shouldBe` 6
