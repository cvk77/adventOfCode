module GiftWrap where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Control.Monad (foldM)

paperNeeded :: Int -> Int -> Int -> Int
paperNeeded l w h = 2*l*w + 2*w*h + 2*h*l + slack
    where slack = product $ take 2 $ sort [l, w, h]

mapOverData :: (Int -> Int -> Int -> Int) -> [String] -> [Maybe Int]
mapOverData f = map ((f $$$) . parts)
    where parts = map read . splitOn "x"

($$$) :: (a -> a -> a -> b) -> [a] -> Maybe b
f $$$ (x:y:z:_) = Just (f x y z)
f $$$ _ = Nothing

sumMaybe :: Num a => [Maybe a] -> Maybe a
sumMaybe = foldM (fmap . (+)) 0

main = do
    s <- readFile "data.txt"
    print $ sumMaybe $ mapOverData paperNeeded $ lines s
