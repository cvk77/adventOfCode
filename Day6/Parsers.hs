module Parsers (
    CommandExpression (..),
    Command (..),
    Coordinates (..),
    parseInstruction
) where

import Text.ParserCombinators.Parsec
import Data.Functor ((<$>), (<$))

data CommandExpression = TurnOn | TurnOff | Toggle deriving (Show, Eq)
data Coordinates = Coordinates Int Int deriving (Show, Eq)
data Command = Command CommandExpression Coordinates Coordinates deriving (Show, Eq)

number :: GenParser Char st Int
number = read <$> many1 digit

command :: GenParser Char st CommandExpression
command
    =   TurnOn  <$ try (string "turn on ")
    <|> TurnOff <$ try (string "turn off ")
    <|> Toggle  <$ try (string "toggle ")

coordinates :: GenParser Char st Coordinates
coordinates = do
    x <- number
    _ <- char ','
    y <- number
    return (Coordinates x y)

instruction :: GenParser Char st Command
instruction = do
    c <- command
    s <- coordinates
    _ <- string " through "
    d <- coordinates
    return (Command c s d)

parseInstruction :: String -> Maybe Command
parseInstruction s = eitherToMaybe $ parse instruction "" s

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x
