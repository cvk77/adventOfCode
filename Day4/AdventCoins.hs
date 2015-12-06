module AdventCoins where

import Data.List
import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.Char8 as B

mine :: Int -> String -> Maybe [Char]
mine n s = find (isCoin n) (map (\i -> s ++ show i) [1..])

isCoin :: Int -> String -> Bool
isCoin n = all (== '0') . take n . show . md5 . B.pack

main :: IO ()
main = do
    -- print $ mine 5 "yzbqklnj"
    print $ mine 6 "yzbqklnj"
