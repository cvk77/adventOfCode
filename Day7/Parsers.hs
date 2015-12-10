module Day7.Parsers where

import Text.ParserCombinators.Parsec
import Data.Word (Word16)
import qualified Data.HashMap.Strict as H

data Wire = Name String | Value Value 
    deriving (Eq, Show)
data Command = Store  Wire
             | Not    Wire
             | Or     Wire Wire
             | And    Wire Wire
             | LShift Wire Wire
             | RShift Wire Wire 
             deriving (Eq, Show)

type Value = Word16
type Circuit = H.HashMap String Command

command :: GenParser Char st (Command, String)
command = do
    return (Or (Value 47) (Value 11), "x")

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

parseCommand :: String -> Maybe (Command, String)
parseCommand s = eitherToMaybe $ parse command "" s

main :: IO ()
main = do
    print $ parseCommand "47 OR 11 -> x"