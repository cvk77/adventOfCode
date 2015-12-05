module Delivery where

import Data.List (nub)

move :: Char -> (Int, Int) -> (Int, Int)
move 'v' (x, y) = (x, y - 1)
move '^' (x, y) = (x, y + 1)
move '<' (x, y) = (x - 1, y)
move '>' (x, y) = (x + 1, y)
move _ p = p

walk :: String -> [(Int,Int)]
walk = f (0, 0)
    where
        f start = foldl step [start]
        step acc@(x:_) a = move a x : acc

housesVisited :: String -> Int
housesVisited = length . nub . walk

main :: IO ()
main = do
    s <- readFile "data.txt"
    print $ housesVisited s
