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
         undefined

main :: IO ()
main = gameLoop initGameState
