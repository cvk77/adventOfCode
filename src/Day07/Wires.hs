module Day07.Wires where

import Data.Bits
import Data.Function.Memoize (memoize)
import qualified Data.Map as Map
import Day07.Parsers

type Circuit = Map.Map String Command

resolve :: Circuit -> String -> Value
resolve circuit = f'
    where
        f s = case circuit Map.! s of
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
    s <- readFile "Day07/data.txt"
    print $ resolve (load $ lines s) "a"
    print $ resolve (foo $ load $ lines s) "a"

    where
        foo c = Map.insert "b" (Store $ Value (resolve c "a")) c
