import Test.Hspec
import AdventCoins (mine)

main = hspec $ do
    describe "adventCoins" $ do
        it "If your secret key is abcdef, the answer is 609043" $ do
            mine "abcdef" `shouldBe` Just "abcdef609043"
        it "If your secret key is pqrstuv, the answer is 1048970" $ do
            mine "pqrstuv" `shouldBe` Just "pqrstuv1048970"
