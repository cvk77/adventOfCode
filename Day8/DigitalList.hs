module Day8.DigitalList where

import Control.Monad (liftM2, ap)

testStrings = [ "\"\""
              , "\"abc\""
              , "\"aaa\\\"aaa\""
              , "\"\\x27\""
              ]

codeLength :: String -> Int
codeLength = length

memoryLength :: String -> Int
memoryLength = f 0
    where
        f acc ('\\':'\\':xs)    = f (acc+1) xs
        f acc ('\"':xs)         = f acc     xs
        f acc ('\\':'x':_:_:xs) = f (acc+1) xs
        f acc (_:xs)            = f (acc+1) xs
        f acc []                = acc

main :: IO ()
main = do
    s <- readFile "Day8/data.txt"
    ls <- return $ lines s
    print $ sum $ map (liftM2 (-) codeLength memoryLength) ls
    print $ sum $ map (ap ((-) . length . show) length)    ls