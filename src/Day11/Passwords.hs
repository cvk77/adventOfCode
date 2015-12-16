module Day11.Passwords where

import Data.List (tails, group)

nextPassword :: String -> [String]
nextPassword cs = filter isValid $ drop 1 $ iterate nextCandidate cs

nextCandidate :: String -> String
nextCandidate = reverse . f .  reverse
    where
        f ('z':c:cs) = 'a' : f (c:cs)
        f ['z']      = "aa"
        f (c:cs)     = succ c : cs

isValid :: String -> Bool
isValid cs = hasStraight cs && hasPair cs && noForbidden cs
    
hasStraight :: String -> Bool
hasStraight cs = any f (tails cs)
    where f (a:b:c:_) = succ a == b && succ b == c
          f _         = False

noForbidden :: String -> Bool
noForbidden cs = 'i' `notElem` cs && 'o' `notElem` cs && 'l' `notElem` cs

hasPair :: String -> Bool
hasPair cs = f $ filter f (group cs)
    where f = (>1) . length
    
main :: IO ()
main = do
    print $ take 2 $ nextPassword "cqjxjnds"
