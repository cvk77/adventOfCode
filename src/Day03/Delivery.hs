module Day03.Delivery where

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
housesVisited = uniqueLength . walk

housesVisited' :: String -> Int
housesVisited' directions = uniqueLength . concatMap walk $ split directions
    where
        split xs = [odds xs, evens xs]
        odds (x:xs) = x : evens xs
        odds _      = []
        evens xs = odds (drop 1 xs)

uniqueLength :: Eq a => [a] -> Int
uniqueLength = length . nub

main :: IO ()
main = do
    s <- readFile "src/Day03/data.txt"
    print $ housesVisited s
    print $ housesVisited' s
