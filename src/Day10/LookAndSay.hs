module Day10.LookAndSay where

import Data.List (group)

lookAndSay :: String -> String
lookAndSay x = concatMap (\x -> (show . length) x ++ take 1 x) $ group x

nthLength :: Int -> Int
nthLength n = length $ iterate lookAndSay "1113222113" !! n

main :: IO ()
main = do
    print $ nthLength 40
    print $ nthLength 50
