module Main where

import Snek
import System.IO

main :: IO ()
main = do
    let ms = MapSize 16 8
    let snek = (Snake E [(Point 8 4)])
    let fud = Point 3 5 -- getRandomNonSnakePoint

    gameLoop ms snek fud

gameLoop :: MapSize -> Snake -> Food -> IO ()
gameLoop ms sn fd = do
    inp <- getLine

    let (Snake sDir sb) = sn

    let dir = (directionFromInput inp sDir)

    let turnedSnake = turnSnake sn dir

    let snakeAteFood = (pointsOverlap sb fd)

    let movedSnek = if snakeAteFood
        then (addSnakeBit turnedSnake)
        else (moveSnake turnedSnake)

    let newFood = if snakeAteFood
        then (Point 1 2) -- getRandomNonSnakePoint
        else fd

    let gm = (createMap ms movedSnek newFood)

    let lostGame = (snakeAteSelf movedSnek)

    if lostGame
        then (putStrLn "You Lost")
        else (putStrLn (displayMap gm))

    gameLoop ms movedSnek newFood

-- getRandomNonSnakePoint :: MapSize ->  Snake -> Point
-- getRandomNonSnakePoint (MapSize xb yb) (Snake d sb)
--     | pointsOverlap sb pt = getRandomNonSnakePoint (Snake d sb)
--     | otherwise = pt
--     where g <- newStdGen
--           x <- randomR (0, xb) g
--           y <- randomR (0, yb) g
--           pt = (Point x y)
