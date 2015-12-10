module Day7.Specs where

import Test.Hspec
import Day7.Parsers
import Day7.Wires (Circuit, load, resolve)
import qualified Data.HashMap.Strict as H

{-# ANN module "HLint: ignore Redundant do" #-}

demoCircuit :: Circuit
demoCircuit = load [ "123 -> x",
                "456 -> y",
                "x AND y -> d",
                "x OR y -> e",
                "x LSHIFT 2 -> f",
                "y RSHIFT 2 -> g",
                "NOT x -> h",
                "NOT y -> i" ]

main :: IO ()
main = hspec $ do
    describe "parse" $ do
        it "should parse Store with value input" $ do
            parse "47 -> x" `shouldBe` ("x", Store (Value 47))
        it "should parse Store with wire input" $ do
            parse "x -> y" `shouldBe` ("y", Store (Name "x"))
        it "should parse Not with value input" $ do
            parse "NOT 47 -> x" `shouldBe` ("x", Not (Value 47))
        it "should parse Not with wire input" $ do
            parse "NOT x -> y" `shouldBe` ("y", Not (Name "x"))
        it "should parse Or with value input" $ do
            parse "47 OR 11 -> x" `shouldBe` ("x", Or (Value 47) (Value 11))
        it "should parse Or with wire input" $ do
            parse "a OR b -> x" `shouldBe` ("x", Or (Name "a") (Name "b"))
        it "should parse And with value input" $ do
            parse "47 AND 11 -> x" `shouldBe` ("x", And (Value 47) (Value 11))
        it "should parse And with wire input" $ do
            parse "a AND b -> x" `shouldBe` ("x", And (Name "a") (Name "b"))
        it "should parse LShift with value input" $ do
            parse "47 LSHIFT 11 -> x" `shouldBe` ("x", LShift (Value 47) (Value 11))
        it "should parse LShift with wire input" $ do
            parse "a LSHIFT b -> x" `shouldBe` ("x", LShift (Name "a") (Name "b"))
        it "should parse RShift with value input" $ do
            parse "47 RSHIFT 11 -> x" `shouldBe` ("x", RShift (Value 47) (Value 11))
        it "should parse RShift with wire input" $ do
            parse "a RSHIFT b -> x" `shouldBe` ("x", RShift (Name "a") (Name "b"))

    describe "load" $ do
        it "should load the demo circuit correctly" $ do
            demoCircuit `shouldBe` H.fromList
                [
                    ("x", Store (Value 123)),            -- 123 -> x
                    ("y", Store (Value 456)),            -- 456 -> y
                    ("d", And (Name "x") (Name "y")),    -- x AND y -> d
                    ("e", Or (Name "x") (Name "y")),     -- x OR y -> e
                    ("f", LShift (Name "x") (Value 2)),  -- x LSHIFT 2 -> f
                    ("g", RShift (Name "y") (Value 2)),  -- y RSHIFT 2 -> g
                    ("h", Not (Name "x")),               -- NOT x -> h
                    ("i", Not (Name "y"))                -- NOT y -> i
                ]

    describe "resolve" $ do
        it "should resolve x: 123 in the demo circuit" $ do
            resolve demoCircuit "x" `shouldBe` 123
        it "should resolve y: 456 in the demo circuit" $ do
            resolve demoCircuit "y" `shouldBe` 456
        it "should resolve d: 72 in the demo circuit" $ do
            resolve demoCircuit "d" `shouldBe` 72
        it "should resolve e: 507 in the demo circuit" $ do
            resolve demoCircuit "e" `shouldBe` 507
        it "should resolve f: 492 in the demo circuit" $ do
            resolve demoCircuit "f" `shouldBe` 492
        it "should resolve g: 114 in the demo circuit" $ do
            resolve demoCircuit "g" `shouldBe` 114
        it "should resolve h: 65412 in the demo circuit" $ do
            resolve demoCircuit "h" `shouldBe` 65412
        it "should resolve i: 65079 in the demo circuit" $ do
            resolve demoCircuit "i" `shouldBe` 65079
