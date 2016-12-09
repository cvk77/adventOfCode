module Day02.BathroomSecurity (puzzle1, puzzle2) where

import Prelude hiding (Left, Right)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map

type Keypad = Map.Map Char (Char, Char, Char, Char)
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
        [('1', ('1', '2', '4', '1'))
        ,('2', ('2', '3', '5', '1'))
        ,('3', ('3', '3', '6', '2'))
        ,('4', ('1', '5', '7', '4'))
        ,('5', ('2', '6', '8', '4'))
        ,('6', ('3', '6', '9', '5'))
        ,('7', ('4', '8', '4', '4'))
        ,('8', ('5', '9', '8', '7'))
        ,('9', ('6', '9', '9', '8'))
        ]

keypad2 :: Keypad
keypad2 = 
    Map.fromList 
        [('1', ('1', '1', '3', '1'))
        ,('2', ('2', '3', '6', '2'))
        ,('3', ('1', '4', '7', '2'))
        ,('4', ('4', '4', '8', '3'))
        ,('5', ('5', '6', '5', '5'))
        ,('6', ('2', '7', 'A', '5'))
        ,('7', ('3', '8', 'B', '6'))
        ,('8', ('4', '9', 'C', '7'))
        ,('9', ('9', '9', '9', '8'))
        ,('A', ('6', 'B', 'A', 'A'))
        ,('B', ('7', 'C', 'D', 'A'))
        ,('C', ('8', 'C', 'C', 'B'))
        ,('D', ('B', 'D', 'D', 'D'))
        ]

step :: Keypad -> Char -> Command -> Char
step kp n = nextKey
    where 
        (u, r, d, l) = Map.findWithDefault (n, n, n, n) n kp
        nextKey Up    = u
        nextKey Down  = d
        nextKey Left  = l
        nextKey Right = r

follow :: Keypad -> Char -> [Command] -> Char
follow kp = foldl (step kp) 

puzzle :: Keypad -> [String] -> Char -> String
puzzle kp cs pos = 
    tail $ scanl (follow kp) pos (map parseLine cs)

puzzle1 :: [String] -> Char -> String
puzzle1 = puzzle keypad1

puzzle2 :: [String] -> Char -> String
puzzle2 = puzzle keypad2 

main :: IO()
main = do
    s <- readFile "src/Day02/data.txt"
    print $ puzzle1 (lines s) '5'
    print $ puzzle2 (lines s) '5'
    