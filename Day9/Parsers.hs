module Day9.Parsers where

type Route = ((String, String), Int)

parse :: String -> Route
parse = f . words
    where
        f [a, "to", b, "=", d] = ((a, b), read d)
        f _ = error "No parse"
