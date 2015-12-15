module Day01.Parantheses where

import Data.List (elemIndex)

delta :: String -> Integer
delta = foldl f 0

steps :: String -> [Integer]
steps = scanl f 0

f :: Num a => a -> Char -> a
f n '(' = n + 1
f n ')' = n - 1
f n _   = n

main :: IO ()
main = do
    s <- readFile "Day01/data.txt"
    print $ delta s
    print $ elemIndex (-1) (steps s)
