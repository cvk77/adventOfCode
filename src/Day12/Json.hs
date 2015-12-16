module Day12.Json where

import Data.List (groupBy)
import Data.Char (isDigit)
import Data.Maybe (catMaybes)

findNumbers :: String -> [Int]
findNumbers xs = catMaybes $ map maybeRead $ groupBy f xs
    where 
        f a b = all (`elem` "0123456789-") [a,b]
        maybeRead s = case reads s of
            [(x, "")] -> Just x
            _         -> Nothing

main :: IO ()
main = do
    s <- readFile "Day12/data.txt"
    print $ sum $ findNumbers s