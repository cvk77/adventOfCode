import Test.Hspec
import NiceStrings (threeVowels, hasDoubles, noForbidden, isNice)

main = hspec $ do
    describe "threeVowels" $ do
        it "works for aei" $ do
            threeVowels "aei" `shouldBe` True
        it "works for xazegov" $ do
            threeVowels "xazegov" `shouldBe` True
        it "works for aeiouaeiouaeiou" $ do
            threeVowels "aei" `shouldBe` True
        it "works for []" $ do
            threeVowels "" `shouldBe` False
        it "works for zzzzzra" $ do
            threeVowels "zzzzzra" `shouldBe` False

    describe "hasDoubles" $ do
        it "works for xx" $ do
            hasDoubles "xx" `shouldBe` True
        it "works for abcdde" $ do
            hasDoubles "abcdde" `shouldBe` True
        it "works for aabbccdd" $ do
            hasDoubles "aabbccdd" `shouldBe` True
        it "works for []" $ do
            hasDoubles "" `shouldBe` False
        it "works for likufanele" $ do
            hasDoubles "likufanele" `shouldBe` False

    describe "noForbidden" $ do
        it "works for aaba" $ do
            noForbidden "aaba" `shouldBe` False
        it "works for acda" $ do
            noForbidden "acda" `shouldBe` False
        it "works for pq" $ do
            noForbidden "pq" `shouldBe` False
        it "works for xxxyyy" $ do
            noForbidden "xxxyyy" `shouldBe` False
        it "works for likufanele" $ do
            noForbidden "likufanele" `shouldBe` True

    describe "isNice" $ do
        it "works for ugknbfddgicrmopn" $ do
            isNice "ugknbfddgicrmopn" `shouldBe` True
        it "works for aaa" $ do
            isNice "aaa" `shouldBe` True
        it "works for jchzalrnumimnmhp" $ do
            isNice "jchzalrnumimnmhp" `shouldBe` False
        it "works for haegwjzuvuyypxyu" $ do
            isNice "haegwjzuvuyypxyu" `shouldBe` False
        it "works for dvszwmarrgswjxmb" $ do
            isNice "dvszwmarrgswjxmb" `shouldBe` False
