import Test.Hspec
import Parantheses (countParantheses)

main = hspec $ do
    describe "count parantheses" $ do
        it "should return 0 for (())" $ do
            countParantheses "(())" `shouldBe` 0
        it "should return 0 for ()()" $ do
            countParantheses "()()" `shouldBe` 0
        it "should return 3 for (((" $ do
            countParantheses "(((" `shouldBe` 3
        it "should return 3 for (()(()(" $ do
            countParantheses "(()(()(" `shouldBe` 3
        it "should return 3 for ))(((((" $ do
            countParantheses "))(((((" `shouldBe` 3
        it "should return -1 for ())" $ do
            countParantheses "())" `shouldBe` -1
        it "should return -1 for ))(" $ do
            countParantheses "))(" `shouldBe` -1
        it "should return -3 for )))" $ do
            countParantheses ")))" `shouldBe` -3
        it "should return -3 for )())())" $ do
            countParantheses ")())())" `shouldBe` -3
