module NiceStrings where

import Data.List (group, isInfixOf)

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

main :: IO()
main = do
    s <- readFile "data.txt"
    print $ length . filter id . map isNice . lines $ s
