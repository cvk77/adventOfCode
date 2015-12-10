module Day7.Wires where

import Data.Bits
import Data.Function.Memoize (memoize)
import qualified Data.Map as Map
import Day7.Parsers

type Circuit = Map.Map String Command

resolve :: Circuit -> String -> Value
resolve cmd = f'
    where
        f s = case cmd Map.! s of
            (Store x)      -> get x
            (Not x)        -> complement $ get x
            (x `And` y )   -> get x .&. get y
            (x `Or` y)     -> get x .|. get y
            (x `LShift` y) -> get x `shiftL` fromIntegral (get y)
            (x `RShift` y) -> get x `shiftR` fromIntegral (get y)
        f' = memoize f

        get (Wire w)  = f' w
        get (Value v) = v

load :: [String] -> Circuit
load s = Map.fromList $ map parse s

main :: IO ()
main = do
    s <- readFile "Day7/data.txt"
    print $ resolve (load $ lines s) "a"
