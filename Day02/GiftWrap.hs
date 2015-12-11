module Day02.GiftWrap where

import Data.List (sort)
import Data.List.Split (splitOn)
import Control.Monad (foldM)

paperNeeded :: Int -> Int -> Int -> Int
paperNeeded l w h = 2*l*w + 2*w*h + 2*h*l + slack
    where slack = product $ shortestSide [l, w, h]

ribbonNeeded :: Int -> Int -> Int -> Int
ribbonNeeded l w h = wrap + bow
    where wrap = sum (shortestSide [l, w, h]) * 2
          bow  = product [l, w, h]

shortestSide :: [Int] -> [Int]
shortestSide = take 2 . sort

mapOverData :: (Int -> Int -> Int -> Int) -> [String] -> [Maybe Int]
mapOverData f = map ((f $$$) . parts)
    where parts = map read . splitOn "x"

($$$) :: (a -> a -> a -> b) -> [a] -> Maybe b
f $$$ (x:y:z:_) = Just (f x y z)
_ $$$ _ = Nothing

sumMaybe :: Num a => [Maybe a] -> Maybe a
sumMaybe = foldM (fmap . (+)) 0

main :: IO ()
main = do
    s <- readFile "Day02/data.txt"
    print $ sumMaybe $ mapOverData paperNeeded $ lines s
    print $ sumMaybe $ mapOverData ribbonNeeded $ lines s
