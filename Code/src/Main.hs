module Main where

import Board
import Display
import Input
import AI

gameLoop :: GameState -> IO ()
gameLoop st
    = do putStrLn (showGameState st)
         putStr "Move: "
         move <- getLine
         let (x, y) = getCoord move
         print (x, y)
         if x == -1 
             then 
                 do putStrLn("That is an invalid coordinate, please check your input and try again")
                    gameLoop st
             else undefined

main :: IO ()
main = gameLoop initGameState
