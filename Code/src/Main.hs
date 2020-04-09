module Main where

import Board
import Display
import Input
import AI

import Data.Maybe

gameLoop :: GameState -> IO ()
gameLoop st
    = do if gameOver (board st) == True 
            then do putStrLn "Game over"
                    return ()
            else 
            
            do  putStrLn ("\n" ++ showGameState st)
                putStrLn ((show (turn st)) ++ "'s turn (" ++ (getPieceStr (turn st)) ++ ")")
                putStr "Move: "
                move <- getLine
                if move == "exit" then return ()
                else if move == "pass" 
                    then do let currentBoard = board st
                            let newState = GameState (Board (size currentBoard)  ((passes currentBoard) + 1) (pieces currentBoard)) (other (turn st))
                            gameLoop newState
                        else  do
                                let (x, y) = getCoord move
                                -- print (x, y)
                                if x == -1
                                    then
                                        do putStrLn("That is an invalid coordinate, please check your input and try again")
                                           gameLoop st
                                    else
                                        do  let new_board = makeMove (board st) (turn st) (x, y)
                                            if isNothing new_board
                                                then
                                                    do putStrLn("That is an invalid move, please try another")
                                                       gameLoop st
                                            else
                                                if turn st == Black then gameLoop (GameState (fromJust new_board) White)
                                                else gameLoop (GameState (fromJust new_board) Black)

main :: IO ()
main = do putStrLn ("Type 'exit' to exit.")
          putStrLn ("Black piece: " ++ (getPieceStr Black))
          putStrLn ("White piece: " ++ (getPieceStr White))
          gameLoop initGameState
