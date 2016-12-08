module Day07.Parsers where

import Data.Word (Word16)

import Text.ParserCombinators.Parsec hiding (parse)
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Language definition

type Value = Word16
type Wire = String

data Input = Wire Wire | Value Value deriving (Eq, Show)

data Command = Store  Input
             | Not    Input
             | Or     Input Input
             | And    Input Input
             | LShift Input Input
             | RShift Input Input
             deriving (Eq, Show)

-- Setup lexer

lexer = Token.makeTokenParser emptyDef
identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
integer    = Token.integer    lexer

-- Parsers

wireName :: GenParser Char st Wire
wireName = identifier

operand :: GenParser Char st Input
operand = choice [ integer  >>= \n -> return $ Value (fromIntegral n)
                 , wireName >>= \s -> return $ Wire s
                 ]

opToWire :: GenParser Char st (Input, Wire)
opToWire = do
    a <- operand
    _ <- reserved "->"
    x <- wireName
    return (a, x)

commandStore :: GenParser Char st (Wire, Command)
commandStore = do
    (a, x) <- opToWire
    return (x, Store a)

commandNot :: GenParser Char st (Wire, Command)
commandNot = do
    _ <- reserved "NOT"
    (a, x) <- opToWire
    return (x, Not a)

command2 :: String -> (Input -> Input -> Command) -> GenParser Char st (Wire, Command)
command2 s c = do
    a <- operand
    _ <- reserved s
    (b, x) <- opToWire
    return (x, c a b)

command :: GenParser Char st (Wire, Command)
command = choice [ try $ command2 "OR" Or
                 , try $ command2 "AND" And
                 , try $ command2 "LSHIFT" LShift
                 , try $ command2 "RSHIFT" RShift
                 , try commandStore
                 , try commandNot
                 ]

parse :: String -> (Wire, Command)
parse s = case Parsec.parse command "" s of
    Right x -> x
    Left _ -> error "no parse"
