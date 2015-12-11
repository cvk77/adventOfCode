module Day05.NiceStrings where

import Data.List (group, isInfixOf, tails)
import Data.List.Split (splitOn)

-- It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
threeVowels :: String -> Bool
threeVowels = (>=3) . length . filter (`elem` "aeiou")

-- It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
hasDoubles :: String -> Bool
hasDoubles = any ((>= 2) . length) . group

-- It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
noForbidden :: String -> Bool
noForbidden s = all (not . (`isInfixOf` s)) ["ab", "cd", "pq", "xy"]

isNice :: String -> Bool
isNice s = all ($ s) [threeVowels, hasDoubles, noForbidden]

-- It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
twoPairs :: String -> Bool
twoPairs word = any (\ pair -> length (splitOn pair word) >= 3) candidates
    where candidates = windowed 2 word

windowed :: Int -> [a] -> [[a]]
windowed x = foldr (zipWith (:)) (repeat []) . take x . tails

-- It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.
triplet :: String -> Bool
triplet (x:y:z:xs)
         | x == z = True
         | otherwise = triplet (y:z:xs)
triplet _ = False

isNice' :: String -> Bool
isNice' s = all ($ s) [twoPairs, triplet]

main :: IO()
main = do
    s <- readFile "Day05/data.txt"
    print $ length . filter id . map isNice  . lines $ s
    print $ length . filter id . map isNice' . lines $ s
