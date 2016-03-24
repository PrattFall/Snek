data Point = Point Int Int deriving (Eq, Show)
data Direction = N | E | S | W deriving (Eq, Show)
data MapSize = MapSize Int Int deriving (Show)
data Snake = Snake Direction [SnakeBit] deriving (Show)

type Cell = String
type MapRow = [Cell]
type Map = [MapRow]

type SnakeBit = Point
type Food = Point

moveBit :: SnakeBit -> Direction -> SnakeBit
moveBit (Point x y) dir =
    case dir of N -> Point x (y-1)
                S -> Point x (y+1)
                E -> Point (x+1) y
                W -> Point (x-1) y

newHead :: Snake -> SnakeBit
newHead (Snake dir s) = (moveBit (head s) dir)

addSnakeBit :: Snake -> Snake
addSnakeBit (Snake dir sb) =
    Snake dir ((newHead (Snake dir sb)):sb)

turnSnake :: Snake -> Direction -> Snake
turnSnake (Snake _ sb) dir = (Snake dir sb)

moveSnake :: Snake -> Snake
moveSnake (Snake dir sb) =
    Snake dir ((newHead (Snake dir sb)):(init sb))

pointsOverlap :: [Point] -> Point -> Bool
pointsOverlap pts tgt = foldl (\acc x ->
    if x == tgt then True else acc) False pts

snakeAteSelf :: Snake -> Bool
snakeAteSelf (Snake _ []) = False
snakeAteSelf (Snake dir (sHead:sTail)) =
    if overlap
        then True
        else (snakeAteSelf (Snake dir sTail))
    where overlap = pointsOverlap sTail sHead

main = do
    let ms = MapSize 8 8

    let snek = (Snake E [(Point 3 3)
                        ,(Point 4 3)
                        ,(Point 5 3)])

    let fud = Point 3 5

    let snek1 = (addSnakeBit (moveSnake (turnSnake (moveSnake snek) W)))

    print snek1
    print (snakeAteSelf (snek1))

