module Day9.Distance where

import Day9.Parsers
import Data.List
import qualified Data.Map as Map

testData = [ "London to Dublin = 464"
           , "London to Belfast = 518"
           , "Dublin to Belfast = 141"
           ]

routes :: [String] -> [Route]
routes = map parse

possibleRoutes :: [Route] -> [[String]]
possibleRoutes = permutations . nub . map fst . Map.keys . Map.fromList

main :: IO ()
main = do
    print $ possibleRoutes $ routes testData