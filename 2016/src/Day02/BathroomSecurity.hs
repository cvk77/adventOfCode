module Day02.BathroomSecurity where

import Prelude hiding (Left, Right)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

type Keypad = Map.Map Int (Int, Int, Int, Int)
data Command = Up | Right | Down | Left 
    deriving (Show)

initialPosition :: Int
initialPosition = 5

parse :: Char -> Maybe Command
parse 'U' = Just Up
parse 'R' = Just Right
parse 'D' = Just Down
parse 'L' = Just Left
parse _   = Nothing

parseLine :: String -> [Command]
parseLine = mapMaybe parse

keypad1 :: Keypad
keypad1 = 
    Map.fromList 
        [(1, (1, 2, 4, 1))
        ,(2, (2, 3, 5, 1))
        ,(3, (3, 3, 6, 2))
        ,(4, (1, 5, 7, 4))
        ,(5, (2, 6, 8, 4))
        ,(6, (3, 6, 9, 5))
        ,(7, (4, 8, 4, 4))
        ,(8, (5, 9, 8, 7))
        ,(9, (6, 9, 9, 8))
        ]

step :: Keypad -> Int -> Command -> Int
step kp n = nextKey
    where 
        (u, r, d, l) = Map.findWithDefault (n, n, n, n) n kp
        nextKey Up    = u
        nextKey Down  = d
        nextKey Left  = l
        nextKey Right = r

follow :: Int -> [Command] -> Int
follow = foldl (step keypad1) 

puzzle1 cs pos = 
    tail $ scanl follow pos (map parseLine cs)

main :: IO()
main = do
    s <- readFile "src/Day02/data.txt"
    print $ puzzle1 (lines s) 5
    