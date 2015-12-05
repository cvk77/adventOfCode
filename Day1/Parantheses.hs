module Parantheses where

import Data.List (elemIndex)

delta :: Num a => [Char] -> a
delta = foldl f 0

steps :: Num a => [Char] -> [a]
steps = scanl f 0

f :: Num a => a -> Char -> a
f n '(' = n + 1
f n ')' = n - 1
f n _   = n

main = do
    s <- readFile "data.txt"
    print $ delta s
    print $ elemIndex (-1) (steps s)
