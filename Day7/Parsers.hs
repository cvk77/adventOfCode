module Day7.Parsers where

import Data.Word (Word16)
import Data.Char (isDigit)

data Input = Wire Wire | Value Value deriving (Eq, Show)
data Command = Store  Input
             | Not    Input
             | Or     Input Input
             | And    Input Input
             | LShift Input Input
             | RShift Input Input
             deriving (Eq, Show)

type Value = Word16
type Wire = String

parse :: String -> (String, Command)
parse s = f $ words s
    where
        f [a,              "->", x] = (x, Store  (toInput a))
        f [   "NOT",    b, "->", x] = (x, Not    (toInput b))
        f [a, "OR",     b, "->", x] = (x, Or     (toInput a) (toInput b))
        f [a, "AND",    b, "->", x] = (x, And    (toInput a) (toInput b))
        f [a, "LSHIFT", b, "->", x] = (x, LShift (toInput a) (toInput b))
        f [a, "RSHIFT", b, "->", x] = (x, RShift (toInput a) (toInput b))
        f _ = error "No parse"

        toInput :: String -> Input
        toInput x = if all isDigit x
                    then Value (read x)
                    else Wire  x
