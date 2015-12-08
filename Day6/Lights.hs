{-# LANGUAGE FlexibleContexts #-}

module Lights where

import Data.Array.IO
import Data.Maybe (mapMaybe)
import Parsers

type Grid a = IOArray (Int, Int) a

createGrid :: Int -> Int -> a -> IO (Grid a)
createGrid w h = newArray ((0,0), (w,h))

execute :: Grid a -> (Grid a -> CommandExpression -> (Int, Int) -> IO ()) -> Command -> IO ()
execute grid f (Command ex a b) =
    mapM_ (f grid ex) (a `to` b)

apply :: Grid Bool -> CommandExpression -> (Int, Int) -> IO ()
apply grid TurnOn  c = writeArray grid c True
apply grid TurnOff c = writeArray grid c False
apply grid Toggle  c = readArray grid c >>= \x -> writeArray grid c (not x)

apply' :: Grid Int -> CommandExpression -> (Int, Int) -> IO ()
apply' grid TurnOn  c = readArray grid c >>= \x -> writeArray grid c (x+1)
apply' grid TurnOff c = readArray grid c >>= \x -> writeArray grid c (if x > 0 then x - 1 else x)
apply' grid Toggle  c = readArray grid c >>= \x -> writeArray grid c (x+2)

to :: (Int, Int) -> (Int, Int) -> [(Int,Int)]
(x1,y1) `to` (x2,y2) = [ (x,y) | x <- [x1..x2], y <- [y1..y2] ]

instructions :: [String] -> [Command]
instructions = mapMaybe parseInstruction

main :: IO ()
main = do
    s <- readFile "data.txt"
    grid <- createGrid 999 999 False
    mapM_ (execute grid apply) (instructions (lines s))
    elems <- getElems grid
    print $ length $ filter id elems

    grid' <- createGrid 999 999 (0 :: Int)
    mapM_ (execute grid' apply') (instructions (lines s))
    elems' <- getElems grid'
    print $ sum elems'
