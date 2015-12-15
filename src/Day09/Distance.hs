module Day09.Distance where

import Day09.Parsers
import Data.List
import qualified Data.Map as Map

type Routes = Map.Map (String, String) Integer

distance :: Routes -> [String] -> Integer
distance ds xs = sum $ map (ds Map.!) (zip xs (tail xs))

routes :: Routes -> [[String]]
routes = permutations . nub . map fst . Map.keys

distances :: [String] -> Routes
distances = Map.fromList . (>>= parse )

main :: IO ()
main = do
    s <- readFile "Day09/data.txt"
    let ds = distances (lines s)
    let rs = routes ds
    let xs = map (distance ds) rs

    print $ minimum xs
    print $ maximum xs
