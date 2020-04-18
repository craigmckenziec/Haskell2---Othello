module Main where

import Board
import Display
import Input
import AI

import System.Environment
import Data.Maybe
import Debug.Trace


gameLoop :: GameState -> IO ()
gameLoop st
    = do if gameOver (board st) == True 
            then do let ammountWhite = evaluate (board st) White
                    let ammountBlack = evaluate (board st) Black
                    if (ammountBlack > ammountWhite) then putStrLn "Black Wins!"
                    else if (ammountWhite > ammountBlack) then putStrLn "White Wins!"
                    else putStrLn "Draw!"
            else do if (turn st) == Black 
                        then if blackPlayer st == Human
                            then humanGameLoop st
                            else aiGameLoop st
                    else if whitePlayer st == Human
                            then humanGameLoop st
                            else aiGameLoop st
                
                    

aiGameLoop :: GameState -> IO ()
aiGameLoop st = do let move = getBestMoveOneDepth (board st) (turn st)
                   let new_board = makeMove (board st) (turn st) (move)
                   if turn st == Black then gameLoop (GameState (fromJust new_board) White (blackPlayer st) (whitePlayer st))
                                       else gameLoop (GameState (fromJust new_board) Black (blackPlayer st) (whitePlayer st))

humanGameLoop :: GameState -> IO ()
humanGameLoop st = do putStrLn ("\n" ++ showGameState st)
                      putStrLn ((show (turn st)) ++ "'s turn (" ++ (getPieceStr (turn st)) ++ ")")
                      putStr "Move: "
                      move <- getLine
                      if move == "exit" then return ()
                        else if move == "pass" 
                        then do let currentBoard = board st
                                let newState = GameState (Board (size currentBoard)  ((passes currentBoard) + 1) (pieces currentBoard)) (other (turn st)) (blackPlayer st) (whitePlayer st) 
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
                                                if turn st == Black then gameLoop (GameState (fromJust new_board) White (blackPlayer st) (whitePlayer st))
                                                else gameLoop (GameState (fromJust new_board) Black (blackPlayer st) (whitePlayer st))


main :: IO ()
main = do putStrLn ("Type 'exit' to exit.")
          putStrLn ("Black piece: " ++ (getPieceStr Black))
          putStrLn ("White piece: " ++ (getPieceStr White))
          
          arguments <- getArgs
          if length arguments == 0 
              then gameLoop initGameState
              else do case buildFromArgs arguments of 
                            Nothing -> do putStrLn "Invalid Entry -- See below for usage"
                                          putStrLn "Usage: ./Main <BlackPlayerType> (\"AI\" || \"Human\") <WhitePlayerType> (\"AI\" || \"Human\") <Size> (8..26)"
                            Just newState -> gameLoop newState



buildFromArgs :: [[Char]] -> Maybe GameState
buildFromArgs arguments = do let argsNO = length arguments
                             if argsNO /= 3 
                                 then Nothing
                                 else do case checkArgs arguments of
                                                Nothing -> Nothing
                                                Just result -> Just result 

checkArgs :: [[Char]] -> Maybe GameState
checkArgs argument = do let argPlayerBlack = checkPlayerType (argument !! 0)
                        let argPlayerWhite = checkPlayerType (argument !! 1)
                        let argSize = checkBoardSize (argument !! 2)
                        if trace' (argPlayerBlack) /= Error && trace' (argPlayerWhite) /= Error 
                                then if trace' (argSize) /= -1
                                         then Just (GameState (getStartBoard argSize) Black argPlayerBlack argPlayerWhite)
                                else Nothing
                        else Nothing

checkPlayerType :: String -> PlayerType
checkPlayerType player = if player == "AI"
                                then AI
                         else if player == "Human"
                                then Human
                                else Error

checkBoardSize :: String -> Int
checkBoardSize size = if checkDigits size == True 
                            then do let sizeInt = (read size :: Int)
                                    if sizeInt < 27 && sizeInt > 7
                                         then sizeInt
                                         else -1
                            else -1

getStartBoard :: Int -> Board
getStartBoard size = do let midpoint = div size 2
                        Board size 0 (getStartPieces midpoint)

getStartPieces :: Int -> [(Position, Col)]
getStartPieces midPoint = do let midPointLess = midPoint - 1 
                             [((midPointLess,midPointLess), Black), ((midPointLess, midPoint), White), ((midPoint,midPointLess), White), ((midPoint,midPoint), Black)]


                             
