module Parantheses where

countParantheses :: [Char] -> Int
countParantheses = foldr f 0
    where f '(' = (+1)
          f ')' = subtract 1
          f _   = id

main = do
    s <- readFile "data.txt"
    print $ countParantheses s
