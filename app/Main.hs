module Main where

import Snek

main :: IO ()
main = do
    let ms = MapSize 8 8

    let snek = (Snake E [(Point 3 3)
                        ,(Point 4 3)
                        ,(Point 5 3)])

    let fud = Point 3 5

    let snek1 = (addSnakeBit (moveSnake (turnSnake (moveSnake snek) W)))
    let gm = (createMap ms snek1 fud)

    putStrLn (displayMap gm)

    print snek1
    print (snakeAteSelf (snek1))
