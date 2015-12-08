module Lights where

import Data.Array.IO
import Data.Maybe (mapMaybe)
import Parsers

type Grid = IOArray (Int, Int) Bool

createGrid :: Int -> Int -> IO Grid
createGrid w h = newArray ((0,0), (w,h)) False

execute :: Grid -> Command -> IO ()
execute grid (Command ex a b) = 
    mapM_ (apply grid ex) (a `to` b)

apply :: Grid -> CommandExpression -> (Int, Int) -> IO ()
apply grid TurnOn c = writeArray grid c True
apply grid TurnOff c = writeArray grid c False
apply grid Toggle c = readArray grid c >>= \x -> writeArray grid c (not x)

to :: (Int, Int) -> (Int, Int) -> [(Int,Int)]
(x1,y1) `to` (x2,y2) = [ (x,y) | x <- [x1..x2], y <- [y1..y2] ]

instructions :: [String] -> [Command]
instructions = mapMaybe parseInstruction

main :: IO ()
main = do
    s <- readFile "data.txt"
    grid <- createGrid 999 999
    mapM_ (execute grid) (instructions (lines s))
    elems <- getElems grid
    print $ length $ filter id $ elems
