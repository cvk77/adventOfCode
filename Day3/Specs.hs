import Test.Hspec
import Delivery (housesVisited)

main = hspec $ do
    describe "housesVisited" $ do
        it "should return 2 for >" $ do
            housesVisited ">" `shouldBe` 2
        it "should return 4 for ^>v<" $ do
            housesVisited "^>v<" `shouldBe` 4
        it "should return 2 for ^v^v^v^v^v" $ do
            housesVisited "^v^v^v^v^v" `shouldBe` 2
