module Main where

import Board
import Display
import Input
import AI

import System.Environment
import Data.Maybe
import Debug.Trace
import System.IO
import Data.List.Split
import Text.Read
import Data.Char
import Control.Concurrent
import Control.Concurrent.Async

hintWaitTime :: Int
hintWaitTime = 30-- time delay before hint is given (seconds)

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
                   if turn st == Black then gameLoop (GameState (fromJust new_board) White (blackPlayer st) (whitePlayer st) (gameMode st) (hintsToggle st) ((previousBoards st) ++ [(board st)]))
                                       else gameLoop (GameState (fromJust new_board) Black (blackPlayer st) (whitePlayer st) (gameMode st) (hintsToggle st) ((previousBoards st) ++ [(board st)]))

humanGameLoop :: GameState -> IO ()
humanGameLoop st = do putStrLn ("\n" ++ showGameState st)
                      putStrLn ((show (turn st)) ++ "'s turn (" ++ (getPieceStr (turn st)) ++ ")")
                      hintGiver <- async (giveHint st getBestMoveOneDepth)
                      putStr "Move: "
                      hFlush stdout
                      move <- getLine
                      cancel hintGiver
                      if move == "exit" then return ()
                        else if move == "pass"
                        then do let currentBoard = board st
                                let newState = GameState (Board (size currentBoard)  ((passes currentBoard) + 1) (pieces currentBoard)) (other (turn st)) (blackPlayer st) (whitePlayer st) (gameMode st) (hintsToggle st) (previousBoards st)
                                gameLoop newState
                        else if move == "options"
                        then optionsLoop st
                        else if move == "undo"
                        then if (length (previousBoards st) == 0) 
                                then do putStrLn("You can't undo on the first move!")
                                        gameLoop st
                                else do let newState = undoMove st
                                        gameLoop newState
                        else  do
                                let (x, y) = getCoord move
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
                                                if turn st == Black then gameLoop (GameState (fromJust new_board) White (blackPlayer st) (whitePlayer st) (gameMode st) (hintsToggle st) ((previousBoards st) ++ [(board st)]))
                                                else gameLoop (GameState (fromJust new_board) Black (blackPlayer st) (whitePlayer st) (gameMode st) (hintsToggle st) ((previousBoards st) ++ [(board st)]))


undoMove :: GameState -> GameState
undoMove (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards) 
                    = do let lastMove = last previousBoards
                         let firstMoves = init previousBoards
                         GameState lastMove (other turn) blackPlayer whitePlayer gameMode hintsToggle firstMoves

giveHint    :: GameState -> (Board -> Col -> Position) -> IO ()
giveHint st getMove = if hintsToggle st == On then do threadDelay (hintWaitTime*10^6)
                                                      let (x, y) = getMove (board st) (turn st)
                                                      putStrLn ("\n\nHint: " ++ [toEnum (65+x)::Char] ++ (show (y)) ++ "\n")
                                                      putStr "Move: "
                                                      hFlush stdout
              else return ()

optionsLoop    :: GameState -> IO ()
optionsLoop st = do putStrLn ("\nOptions")
                    putStrLn ("Use '<option>=<selection>' to change an option")
                    putStrLn ("Use command 'restart' to restart game")
                    putStrLn ("Use command 'resume' to return to game")
                    putStrLn ("black player (AI/Human): " ++ show (blackPlayer st))
                    putStrLn ("white player (AI/Human): " ++ show (whitePlayer st))
                    putStrLn ("size (8..26): " ++ show (size (board st)))
                    putStrLn ("game mode (Othello/Reversi): " ++ show (gameMode st))
                    putStrLn ("hints (On/Off): " ++ show (hintsToggle st))
                    putStr "option: "
                    hFlush stdout
                    input <- getLine
                    checkOption input st

checkOption       :: String -> GameState -> IO()
checkOption xs st = do let optionChange = splitOn "=" xs
                       if length optionChange == 2 then setOption (optionChange !! 0) (optionChange !! 1) st
                       else if xs == "resume" then
                               if (gameMode st) == Reversi && (length (pieces (board (st)))) < 4 then startReversi st
                               else gameLoop st
                       else if xs == "restart" then startGame (blackPlayer st) (whitePlayer st) (size (board st)) (gameMode st) (hintsToggle st)
                       else optionsLoop st

setOption                             :: String -> String -> GameState -> IO()
setOption "black player" selection st = if selection == "AI" then optionsLoop (GameState (board st) (turn st) AI (whitePlayer st) (gameMode st) (hintsToggle st) (previousBoards st))
                                        else if selection == "Human" then optionsLoop (GameState (board st) (turn st) Human (whitePlayer st) (gameMode st) (hintsToggle st) (previousBoards st))
                                        else do putStrLn "\nerror: black player can either be 'AI' or 'Human'"
                                                optionsLoop st
setOption "white player" selection st = if selection == "AI" then optionsLoop (GameState (board st) (turn st) (blackPlayer st) AI (gameMode st) (hintsToggle st) (previousBoards st))
                                        else if selection == "Human" then optionsLoop (GameState (board st) (turn st) (blackPlayer st) Human (gameMode st) (hintsToggle st) (previousBoards st))
                                        else do putStrLn "\nerror: black player can either be 'AI' or 'Human'"
                                                optionsLoop st
setOption "size" selection st = do let new_size = readMaybe selection :: Maybe Int
                                   if new_size /= Nothing && 8 <= fromJust new_size && fromJust new_size <=26 then
                                      if fromJust new_size /= size (board st) then startGame (blackPlayer st) (whitePlayer st) (fromJust new_size) (gameMode st) (hintsToggle st)
                                      else do putStrLn ("\nerror: the size is already " ++ (show new_size))
                                              optionsLoop st
                                   else do putStrLn "\nerror: size must be an integer between 8 and 26 (inclusive)"
                                           optionsLoop st
setOption "game mode" selection st = if selection == "Othello" then
                                        if gameMode st /= Othello then startGame (blackPlayer st) (whitePlayer st) (size (board st)) Othello (hintsToggle st)
                                        else do putStrLn "\nerror: you are already playing Othello however you can restart using the 'restart' command in the options menu"
                                                optionsLoop st
                                     else if selection == "Reversi" then
                                        if gameMode st /= Reversi then startGame (blackPlayer st) (whitePlayer st) (size (board st)) Reversi (hintsToggle st)
                                        else do putStrLn "\nerror: you are already playing Reversi however you can restart using the 'restart' command in the options menu"
                                                optionsLoop st
                                     else do putStrLn "\nerror: game mode can either be 'Othello' or 'Reversi'"
                                             optionsLoop st
setOption "hints" selection st = if selection == "On" then optionsLoop (GameState (board st) (turn st) (blackPlayer st) (whitePlayer st) (gameMode st) On (previousBoards st))
                                 else if selection == "Off" then optionsLoop (GameState (board st) (turn st) (blackPlayer st) (whitePlayer st) (gameMode st) Off (previousBoards st))
                                 else do putStrLn "\nerror: hintsToggle can either be 'On' or 'Off'"
                                         optionsLoop st


getPlayerType    :: GameState -> PlayerType
getPlayerType st | turn st == Black = blackPlayer st
                 | turn st == White = whitePlayer st



main :: IO ()
main = do putStrLn ("Type 'exit' to exit or 'options' to change options.")
          putStrLn ("Black piece: " ++ (getPieceStr Black))
          putStrLn ("White piece: " ++ (getPieceStr White))

          arguments <- getArgs
          if length arguments == 0
              then gameLoop initGameState
              else do case buildFromArgs arguments of
                            Nothing -> do putStrLn "Invalid Entry -- See below for usage"
                                          putStrLn "Usage: ./Main <BlackPlayerType> (\"AI\" || \"Human\") <WhitePlayerType> (\"AI\" || \"Human\") <Size> (8..26) <GameMode> (\"Othello\" || \"Reversi\") <HintsToggle> (\"On\" || \"Off\")"
                            Just playGame -> playGame



buildFromArgs :: [[Char]] -> Maybe (IO ())
buildFromArgs arguments = do let argsNO = length arguments
                             if argsNO /= 5
                                 then Nothing
                                 else do case checkArgs arguments of
                                                Nothing -> Nothing
                                                Just result -> Just result

checkArgs :: [[Char]] -> Maybe (IO ())
checkArgs argument = do let argPlayerBlack = checkPlayerType (argument !! 0)
                        let argPlayerWhite = checkPlayerType (argument !! 1)
                        let argSize = checkBoardSize (argument !! 2)
                        let argGameMode = checkGameMode (argument !! 3)
                        let argHintsToggle = checkHintsToggle (argument !! 4)
                        if trace' (argPlayerBlack) /= PLayerTypeError && trace' (argPlayerWhite) /= PLayerTypeError
                                then if trace' (argSize) /= -1
                                         then if trace' (argGameMode) /= GameModeError
                                                 then if trace' (argHintsToggle) /= HintsToggleError
                                                         then Just (startGame argPlayerBlack argPlayerWhite argSize argGameMode argHintsToggle)
                                                 else Nothing
                                         else Nothing
                                else Nothing
                        else Nothing

checkPlayerType :: String -> PlayerType
checkPlayerType player = if player == "AI"
                                then AI
                         else if player == "Human"
                                then Human
                                else PLayerTypeError

checkBoardSize :: String -> Int
checkBoardSize size = if checkDigits size == True
                            then do let sizeInt = (read size :: Int)
                                    if sizeInt < 27 && sizeInt > 7
                                         then sizeInt
                                         else -1
                            else -1

startGame                                                                      :: PlayerType -> PlayerType -> Int -> GameMode -> HintsToggle -> IO ()
startGame blackPlayerType whitePlayerType boardSize Othello hintsToggleSetting = do let midPoint = boardSize `div` 2
                                                                                    let midPointLess = midPoint - 1
                                                                                    let gameBoard = Board boardSize 0 [((midPointLess, midPointLess), Black), ((midPointLess, midPoint), White), ((midPoint, midPointLess), White), ((midPoint, midPoint), Black)]
                                                                                    gameLoop (GameState gameBoard Black blackPlayerType whitePlayerType Othello hintsToggleSetting [])
startGame blackPlayerType whitePlayerType boardSize Reversi hintsToggleSetting = startReversi (GameState (Board boardSize 0 []) Black blackPlayerType whitePlayerType Reversi hintsToggleSetting [])



startReversi                                :: GameState -> IO ()
startReversi st | getPlayerType st == Human = do putStrLn ("\n" ++ showGameState st)
                                                 putStrLn ((show (turn st)) ++ "'s turn (" ++ (getPieceStr (turn st)) ++ ")")
                                                 hintGiver <- async (giveHint st getBestReversiInitialMove)
                                                 putStr "Move: "
                                                 hFlush stdout
                                                 move <- getLine
                                                 cancel hintGiver
                                                 if move == "exit" then return ()
                                                 else if move == "options" then optionsLoop st
                                                 else do let (x, y) = getCoord move
                                                         if x == -1 then do putStrLn("That is an invalid coordinate, please check your input and try again")
                                                                            startReversi st
                                                         else do let new_board = makeReversiInitialMove (board st) (turn st) (x, y)
                                                                 if isNothing new_board then do putStrLn("That is an invalid move, in Reversi, the first 4 moves must be within the center 2x2 square. Please try another")
                                                                                                startReversi st
                                                                 else if length (pieces (fromJust new_board)) == 4 then
                                                                         if turn st == Black then gameLoop (GameState (fromJust new_board) White (blackPlayer st) (whitePlayer st) (gameMode st) (hintsToggle st) [])
                                                                         else gameLoop (GameState (fromJust new_board) Black (blackPlayer st) (whitePlayer st) (gameMode st) (hintsToggle st) [])
                                                                 else
                                                                     if turn st == Black then startReversi (GameState (fromJust new_board) White (blackPlayer st) (whitePlayer st) (gameMode st) (hintsToggle st) [])
                                                                     else startReversi (GameState (fromJust new_board) Black (blackPlayer st) (whitePlayer st) (gameMode st) (hintsToggle st) [])
                | otherwise = do let move = getBestReversiInitialMove (board st) (turn st)
                                 let new_board = fromJust (makeReversiInitialMove (board st) (turn st) move)
                                 if length (pieces new_board) == 4 then
                                    if turn st == Black then gameLoop (GameState new_board White (blackPlayer st) (whitePlayer st) (gameMode st) (hintsToggle st) [])
                                    else gameLoop (GameState new_board Black (blackPlayer st) (whitePlayer st) (gameMode st) (hintsToggle st) [])
                                 else
                                    if turn st == Black then startReversi (GameState new_board White (blackPlayer st) (whitePlayer st) (gameMode st) (hintsToggle st) [])
                                    else startReversi (GameState new_board Black (blackPlayer st) (whitePlayer st) (gameMode st) (hintsToggle st) [])



checkGameMode :: String -> GameMode
checkGameMode gameMode = if gameMode == "Othello"
                            then Othello
                         else if gameMode == "Reversi"
                            then Reversi
                         else GameModeError

checkHintsToggle :: String -> HintsToggle
checkHintsToggle toggle = if toggle == "On"
                             then On
                          else if toggle == "Off"
                             then Off
                          else HintsToggleError
