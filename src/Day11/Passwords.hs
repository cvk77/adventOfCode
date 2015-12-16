module Day11.Passwords where

import Data.List (tails, group)

nextPassword :: String -> [String]
nextPassword = filter isValid . drop 1 . iterate nextCandidate

nextCandidate :: String -> String
nextCandidate = reverse . f .  reverse
    where
        f ('z':c:cs) = 'a' : f (c:cs)
        f ['z']      = "aa"
        f (c:cs)     = succ c : cs

isValid :: String -> Bool
isValid cs = hasStraight cs && hasPair cs && noForbidden cs
    
hasStraight :: String -> Bool
hasStraight = any f . tails
    where f (a:b:c:_) = succ a == b && succ b == c
          f _         = False

noForbidden :: String -> Bool
noForbidden cs = 'i' `notElem` cs && 'o' `notElem` cs && 'l' `notElem` cs

hasPair :: String -> Bool
hasPair = f . filter f . group
    where f = (>1) . length
    
main :: IO ()
main = do
    print $ take 2 $ nextPassword "cqjxjnds"
