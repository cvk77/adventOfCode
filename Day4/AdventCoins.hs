{-# LANGUAGE OverloadedStrings #-}

module Day4.AdventCoins where

import Crypto.Hash (Digest, MD5, hash, digestToHexByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.List (find)

mine :: Int -> ByteString -> Maybe Int
mine len s = find (isCoin len . candidate) [1..]
    where
        candidate n = s `B.append` B.pack (show n)

isCoin :: Int -> ByteString -> Bool
isCoin len s = prefix `B.isPrefixOf` digest
    where
        prefix = B.replicate len '0'
        digest = digestToHexByteString (hash s :: (Digest MD5))

main :: IO ()
main = do
    print $ mine 5 "yzbqklnj"
    print $ mine 6 "yzbqklnj"
