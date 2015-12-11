module Day9.Distance where

import Day9.Parsers
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
    s <- readFile "Day9/data.txt"
    ds <- return $ distances (lines s)
    rs <- return $ routes ds
    xs <- return $ map (distance ds) rs

    print $ minimum $ xs
    print $ maximum $ xs
