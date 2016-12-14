module Day03.Triangles where

import Control.Arrow ((>>>))
import Data.List (sort)

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = a + b > c

toTuple3 :: [a] -> (a, a, a)
toTuple3 (a:b:c:_) = (a, b, c)

parseList :: String -> [(Int, Int, Int)]
parseList = lines >>> map (words >>> map read >>> sort >>> toTuple3)

uncurry3  :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x,y,z) =  f x y z

main :: IO ()
main = do
    s <- readFile "src/Day03/data.txt"
    let ws = parseList s
    print ws
    let xs = map (uncurry3 isTriangle) ws
    print $ length $ filter id xs