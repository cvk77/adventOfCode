module Day01.EasterBunnyHeadquarters (puzzle1, puzzle2) where

import Data.Maybe (mapMaybe)
import Control.Arrow ((>>>))
import Prelude hiding (Left, Right)
import Data.List.Split (splitOn)
import Enum.Bounded (predB, succB)
import qualified Data.Set as Set

data Direction = Left | Right deriving (Show)
data Bearing = North | East | South | West deriving (Show, Enum, Bounded, Eq)
data Command = Command Direction Distance deriving (Show)
type Position = (Int, Int) 
type Distance = Int 
data State = State 
    { bearing  :: Bearing
    , position :: Position
    , history  :: [Position] 
    } 
    deriving (Show)

initialState :: State
initialState = State North (0,0) [(0,0)]

-- | The 'parse' function parses a single command string into a 'Command'.
parse :: String -> Maybe Command
parse (direction:distance) 
    | direction == 'L'  = Just $ Command Left distance' 
    | direction == 'R'  = Just $ Command Right distance'
    | otherwise = Nothing
    where
        distance' = read distance

-- | The 'parseAll' function parses all commands in a comma-separated list and 
-- returns a list of valid 'Command's.  
parseAll :: String -> [Command]
parseAll s = mapMaybe parse $ splitOn ", " s

-- | The 'step' function takes a 'Bearing' and a 'Position' and returns the new 
-- 'Position' after taking a single step.
step :: Bearing -> Position -> Position
step North (x, y) = (x,   y+1)
step East  (x, y) = (x+1, y  )
step South (x, y) = (x,   y-1)
step West  (x, y) = (x-1, y  )

-- | The 'walk' function takes a 'Distance' and a 'State' and returns the new
-- state after walking the given distance.
walk :: Distance -> State -> State
walk 0 state = state
walk distance (State bearing position history) = 
    walk (distance-1) (State bearing newPosition (newPosition:history)) 
    where 
        newPosition = step bearing position

-- | The 'turn' function takes a 'Direction' and a 'State' and returns the new
-- state after turning in the given direction.
turn :: Direction -> State -> State
turn Left  state = state { bearing = predB (bearing state)} 
turn Right state = state { bearing = succB (bearing state)}

-- | The 'move' function takes a 'Command' and a 'State' and 'turn's into the
-- 'Direction' indicated in the command and then returns the new State after 
-- 'walk'ing the distance given in the command.
move :: Command -> State -> State
move (Command direction distance) = 
    turn direction >>> walk distance 

-- | The 'follow' function takes a list of 'Command's and a 'State' and returns
-- the new state after applying all commands in order.
follow :: [Command] -> State -> State
follow cs state = foldl (flip move) state cs

-- | The 'manhattanDistance' function takes a 'Position' and returns the 
-- manhattan distance to the origin (0,0).
manhattanDistance :: Position -> Int
manhattanDistance (x, y) = abs x + abs y

dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs Set.empty
  where dup' [] _ = Nothing
        dup' (x:xs) s = if Set.member x s 
                           then Just x
                           else dup' xs (Set.insert x s)


puzzle1 :: String -> Int
puzzle1 instructions =
    follow (parseAll instructions)  
    >>> history 
    >>> head 
    >>> manhattanDistance
    $ initialState

puzzle2 :: String -> Maybe Int
puzzle2 instructions =
    follow (parseAll instructions)
    >>> history
    >>> reverse >>> dup 
    >>> fmap manhattanDistance
    $ initialState
     

main :: IO ()
main = do
    s <- readFile "src/Day01/data.txt"
    print $ puzzle1 s 
    print $ puzzle2 s 
