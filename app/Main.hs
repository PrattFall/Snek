module Main where

import System.IO
import System.Random

import Snek

main :: IO ()
main = do
    gen <- newStdGen

    let ms = MapSize 16 8

    let snek = (Snake E [(Point 8 4)])

    let fud = getRandomNonSnakePoint gen ms snek

    gameLoop ms snek fud

gameLoop :: MapSize -> Snake -> Food -> IO ()
gameLoop ms sn fd = do
    inp <- getLine
    gen <- newStdGen

    let (Snake sDir sb) = sn

    let dir = (directionFromInput inp sDir)

    let turnedSnake = turnSnake sn dir

    let snakeAteFood = (elem fd sb)

    let movedSnek = if snakeAteFood
        then (addSnakeBit turnedSnake)
        else (moveSnake turnedSnake)

    let newFood = if snakeAteFood
        then getRandomNonSnakePoint gen ms movedSnek
        else fd

    let gm = (createMap ms movedSnek newFood)

    let lostGame = (snakeAteSelf movedSnek)

    if lostGame
        then (lostMode movedSnek)
        else (putStrLn (displayMap gm))

    gameLoop ms movedSnek newFood

lostMode :: Snake -> IO ()
lostMode sn = do
    let numSB = (show (countSnakeBits sn))
    putStrLn "Snek is Dead."
    putStrLn ("You ended up with " ++ numSB ++ " points!")
    putStrLn "Press any key to start over."
    main

getRandomNonSnakePoint :: StdGen -> MapSize -> Snake -> Point
getRandomNonSnakePoint g (MapSize xb yb) (Snake dir sb)
    | elem pt sb =
        getRandomNonSnakePoint tG (MapSize xb yb) (Snake dir sb)
    | otherwise = pt
    where (x, sG) = randomR (0, (xb-1)) g
          (y, tG) = randomR (0, (yb-1)) sG
          pt = (Point x y)
