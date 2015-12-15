module Day09.Parsers where

parse :: String -> [((String, String), Integer)]
parse = f . words
    where
        f [a, "to", b, "=", d] = [((a, b), read d), ((b, a), read d)]
        f _ = error "No parse"
