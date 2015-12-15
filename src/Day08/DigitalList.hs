module Day08.DigitalList where

import Control.Monad (liftM2)

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

encodedLength :: String -> Int
encodedLength = length . show

main :: IO ()
main = do
    s <- readFile "Day08/data.txt"
    ls <- return $ lines s
    print $ sum $ map (liftM2 (-) codeLength memoryLength) ls
    print $ sum $ map (liftM2 (-) encodedLength codeLength) ls
