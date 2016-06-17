module Snek
( Point(..)
, Direction(..)
, MapSize(..)
, Snake(..)
, GameMap(..)
, SnakeBit
, Food
, addSnakeBit
, turnSnake
, moveSnake
, snakeAteSelf
, createMap
, displayMap
, directionFromInput
, countSnakeBits
, isInBounds
) where

data Point = Point Int Int deriving (Eq, Show)
data Direction = N | E | S | W deriving (Eq, Show)
data MapSize = MapSize Int Int deriving (Show)
data Snake = Snake Direction [SnakeBit] deriving (Show)
data GameMap = GameMap MapSize [MapRow] deriving (Show)

type Cell = String
type MapRow = [Cell]

type SnakeBit = Point
type Food = Point

moveBit :: SnakeBit -> Direction -> SnakeBit
moveBit (Point x y) dir =
    case dir of N -> Point x (y-1)
                S -> Point x (y+1)
                E -> Point (x+1) y
                W -> Point (x-1) y

newHead :: Snake -> SnakeBit
newHead (Snake dir s) = moveBit (head s) dir

addSnakeBit :: Snake -> Snake
addSnakeBit (Snake dir sb) =
    Snake dir (newHead (Snake dir sb):sb)

turnSnake :: Snake -> Direction -> Snake
turnSnake (Snake _ sb) dir = Snake dir sb

moveSnake :: Snake -> Snake
moveSnake (Snake dir sb) =
    Snake dir (newHead (Snake dir sb):init sb)

snakeAteSelf :: Snake -> Bool
snakeAteSelf (Snake _ []) = False
snakeAteSelf (Snake dir (sHead:sTail)) =
    overlap || snakeAteSelf (Snake dir sTail)
    where overlap = sHead `elem` sTail

createCell :: Int -> Int -> [SnakeBit] -> Food -> Cell
createCell x y sb fd
    | Point x y `elem` sb = "S"
    | Point x y == fd     = "F"
    | otherwise           = "."

createMapRow :: Int -> Int -> [SnakeBit] -> Food -> MapRow
createMapRow xb y sb fd =
    [createCell x y sb fd | x <- [0..(xb-1)]]

createMap :: MapSize -> Snake -> Food -> GameMap
createMap (MapSize xb yb) (Snake _ sb) fd =
    GameMap (MapSize xb yb) row
    where row = [createMapRow xb y sb fd | y <- [0..(yb-1)]]

displayMap :: GameMap -> String
displayMap (GameMap (MapSize xb yb) mp) =
    concatMap (\x -> concat x ++ "\n") mp

countSnakeBits :: Snake -> Int
countSnakeBits (Snake _ sb) = length sb

directionFromInput :: Char -> Direction -> Direction
directionFromInput 'w' _ = N
directionFromInput 's' _ = S
directionFromInput 'a' _ = W
directionFromInput 'd' _ = E
-- Vim keybinds :)
directionFromInput 'k' _ = N
directionFromInput 'j' _ = S
directionFromInput 'h' _ = W
directionFromInput 'l' _ = E
directionFromInput _ dir = dir

isInBounds :: Snake -> MapSize -> Bool
isInBounds (Snake _ (Point x y:_)) (MapSize xb yb) =
    x >= 0 && y >= 0 && x < xb && y < yb
