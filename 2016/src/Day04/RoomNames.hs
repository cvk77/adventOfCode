module Day04.RoomNames (Room(..), puzzle1, puzzle2) where

import Data.List
import Data.Ord
import Data.Char
import Data.Maybe
import Data.Monoid
import Text.Parsec
import Control.Monad ((>=>))
import Control.Arrow ((>>>))

freq :: Ord a => [a] -> [(Int, a)]
freq xs = sortBy (mconcat [flip (comparing fst), comparing snd]) [ (length $ filter (== c) xs, c) | c <- nub xs]

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

type RoomName = String
type SectorId = Int
type Checksum = String

data Room = Room 
    { roomName :: RoomName
    , sectorId :: SectorId
    , checksum :: Checksum 
    }
    deriving (Show, Eq)

getChecksum :: Room -> Checksum
getChecksum name = take 5 $ map snd $ freq name'
    where name' = filter (/= '-') (roomName name) 

checkSectorId :: Room -> Maybe Room
checkSectorId room = if   calculatedChecksum == actualChecksum 
                     then Just room
                     else Nothing
    where calculatedChecksum = getChecksum room
          actualChecksum = checksum room 

toMaybe :: Either a b -> Maybe b
toMaybe (Left _) = Nothing
toMaybe (Right x) = Just x

roomNameAndSectorId = do
    n  <- many1 (many1 letter <* char '-')
    s  <- read <$> many1 digit
    return (intercalate "-" n, s) 

encryptedNameParser = do
    (n, s) <- roomNameAndSectorId
    cs <- char '[' *> many1 letter <* char ']'
    return $ Room n s cs

rot :: Int -> String -> String
rot n = map (shift n)
    where
        shift n c | isLower c = chr' ((ord' c + n) `mod` 26)
                  | c == '-'  = ' '
                  | otherwise = c
        ord' c = ord c - ord 'a'
        chr' n = chr (ord 'a' + n)

checkRoom :: String -> Maybe Room 
checkRoom = parseRoom >=> checkSectorId
    where
        parseRoom = toMaybe . parse encryptedNameParser ""

puzzle1 :: String -> Maybe Int
puzzle1 = checkRoom >=> sectorId >>> return

puzzle2 :: String -> Maybe Room
puzzle2 = checkRoom >=> decodeName
    where 
        decodeName (Room n sId cs) = return $ Room (rot sId n) sId cs

main :: IO ()
main = do
    s <- readFile "src/Day04/data.txt"
    let rs = lines s
    print $ sum $ mapMaybe puzzle1 rs
    print $ filter (\room -> roomName room == "northpole object storage") $ mapMaybe puzzle2 rs

