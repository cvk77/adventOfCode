import Test.Hspec

import Parsers

main :: IO ()
main = hspec $ do
    describe "parseInstruction" $ do
        it "should parse 'turn on 0,0 through 999,999' correctly" $ do
            parseInstruction "turn on 0,0 through 999,999" `shouldBe`
                Just (Command TurnOn (Coordinates 0 0) (Coordinates 999 999))
        it "should parse 'toggle 0,0 through 999,0' correctly" $ do
            parseInstruction "toggle 0,0 through 999,0" `shouldBe`
                Just (Command Toggle (Coordinates 0 0) (Coordinates 999 0))
        it "should parse 'turn off 499,499 through 500,500' correctly" $ do
            parseInstruction "turn off 499,499 through 500,500" `shouldBe`
                Just (Command TurnOff (Coordinates 499 499) (Coordinates 500 500))
