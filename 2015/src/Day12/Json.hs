module Day12.Json where

import Data.List (groupBy)
import Data.Char (isDigit)
import Data.Maybe (catMaybes)

sumNumbers :: String -> Int -> Int
sumNumbers [] acc = acc
sumNumbers xs acc = sumNumbers rest (acc + n)
    where
        (n, rest) = (parseNumber . skip) xs

parseNumber :: String -> (Int, String)
parseNumber "" = (0, "")
parseNumber xs = (read n, rest)
    where 
        (n, rest) = span isNumeric xs

skip :: String -> String
skip = dropWhile (not . isNumeric)

isNumeric :: Char -> Bool
isNumeric x = x == '-' || isDigit x

main :: IO ()
main = do
    s <- readFile "src/Day12/data.txt"
    print $ sumNumbers s 0
