module Main where

import Board
import Display
import Input
import AI

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.List.Split
import Data.Maybe
import Debug.Trace
import System.Environment
import System.IO
import Text.Read
import UI.NCurses

hintWaitTime :: Integer
hintWaitTime = 30-- time delay before hint is given (seconds)

data Action =  Options | Pass | Quit | Move String
  deriving Eq

gameLoop :: GameState -> Window -> Curses ()
gameLoop st w
    = do if gameOver (board st) == True
            then do let ammountWhite = evaluate (board st) White
                    let ammountBlack = evaluate (board st) Black
                    updateWindow w $ do clear
                                        moveCursor 0 0
                    if (ammountBlack > ammountWhite) then do updateWindow w (drawString "Black Wins!") -- *****sort out game over screen *************
                    else if (ammountWhite > ammountBlack) then updateWindow w (drawString "White Wins!") -- *****sort out game over screen *************
                    else updateWindow w (drawString "Draw!") -- *****sort out game over screen *************
            else do if (turn st) == Black
                        then if blackPlayer st == Human
                            then humanGameLoop st w
                            else aiGameLoop st w
                    else if whitePlayer st == Human
                            then humanGameLoop st w
                            else aiGameLoop st w


aiGameLoop :: GameState -> Window -> Curses ()
aiGameLoop st w = do let move = getBestMoveOneDepth (board st) (turn st)
                     let new_board = makeMove (board st) (turn st) (move)
                     if turn st == Black then gameLoop st {board = fromJust new_board, turn = White} w
                                         else gameLoop st {board = fromJust new_board, turn = Black} w

humanGameLoop :: GameState -> Window -> Curses ()
humanGameLoop st w = do updateWindow w $ do clear
                                            moveCursor 0 0
                                            drawString (showGameState st)
                                            drawString ((show (turn st)) ++ "'s turn (" ++ (getPieceStr (turn st)) ++ ")\n")
                                            drawString ("move: ")
                        render
                        move <- getMove st w getBestMoveOneDepth
                        if move == Quit then return ()
                          else if move == Pass
                              then do let currentBoard = board st
                                      let newState = st {board = Board (size currentBoard)  ((passes currentBoard) + 1) (pieces currentBoard)}
                                      gameLoop newState w
                          else if move == Options
                              then optionsLoop st w gameLoop
                          else  do
                                  let (x, y) = getCoord ((\(Move coordinateString) -> coordinateString) move)
                                  if x == -1
                                      then
                                          do updateWindow w $ do moveCursor (toInteger (size (board st)) + 3) 0
                                                                 clearLine
                                                                 drawString "That is an invalid coordinate, please check your input and try again" -- implement proper invalid screen
                                             render
                                             gameLoop st w
                                      else
                                          do  let new_board = makeMove (board st) (turn st) (x, y)
                                              if isNothing new_board
                                                  then
                                                      do updateWindow w $ do moveCursor (toInteger (size (board st)) + 2) 0
                                                                             clearLine
                                                                             drawString "That is an invalid move, please try another" -- implement proper invalid screen
                                                         render
                                                         gameLoop st w
                                              else
                                                  if turn st == Black then gameLoop (st {board = fromJust new_board, turn = White}) w
                                                  else gameLoop (st {board = fromJust new_board, turn = Black}) w


getMove :: GameState -> Window -> (Board -> Col -> Position) -> Curses Action
getMove st w getBestMove = loop "" where
    loop input = do ev <- getEvent w (Just (hintWaitTime*10^3))
                    case ev of
                         Nothing -> do giveHint st w input getBestMove
                                       loop input
                         Just (EventCharacter '\ESC') -> return Options
                         Just (EventCharacter '\n') -> return (Move input)
                         Just (EventSpecialKey KeyBackspace)
                           | length input > 0 -> do updateWindow w (drawString "\b \b")
                                                    render
                                                    loop (init input)
                           | otherwise -> loop ""
                         Just (EventCharacter c)
                           | isAlphaNum c -> do updateWindow w (drawString [c])
                                                render
                                                loop (input ++ [c])
                           | otherwise -> loop input
                         Just _ -> loop input

giveHint :: GameState -> Window -> String -> (Board -> Col -> Position) -> Curses ()
giveHint st w input getBestMove | hintsToggle st == On = do let (x, y) = getBestMove (board st) (turn st)
                                                            updateWindow w $ do moveCursor (toInteger (size (board st)) + 2) 0
                                                                                clearLine
                                                                                drawString ("Hint: " ++ [toEnum (65+x)::Char] ++ (show (y)))
                                                                                drawString "\nmove: "
                                                            render
                                                            when (length input > 0) (do updateWindow w (drawString input)
                                                                                        render)
                                | otherwise = return ()

optionsLoop      :: GameState -> Window -> (GameState -> Window -> Curses ()) -> Curses ()
optionsLoop st w returnScreen = do updateWindow w $ do clear
                                                       moveCursor 0 0
                                                       drawString "Options\n\n"
                                                       drawString ("black player (AI/Human): " ++ show (blackPlayer st) ++ "\n")
                                                       drawString ("white player (AI/Human): " ++ show (whitePlayer st) ++ "\n")
                                                       drawString ("size (8..26): " ++ show (size (board st)) ++ "\n")
                                                       drawString ("game mode (Othello/Reversi): " ++ show (gameMode st) ++ "\n")
                                                       drawString ("hints (On/Off): " ++ show (hintsToggle st) ++ "\n\n")
                                                       drawString "change black player: b\n"
                                                       drawString "change white player: w\n"
                                                       drawString "change size: s\n"
                                                       drawString "change game mode: g\n"
                                                       drawString "toggle hints: h\n\n"
                                                       drawString "resume: escape\n"
                                                       drawString "restart: r\n"
                                                       drawString "quit: q\n"
                                   render
                                   loop where
                                       loop = do ev <- getEvent w Nothing
                                                 case ev of
                                                      Nothing -> loop
                                                      Just (EventCharacter c)
                                                        | c == 'b' || c == 'B' -> if (blackPlayer st) == AI then optionsLoop (st {blackPlayer = Human}) w returnScreen
                                                                                  else optionsLoop (st {blackPlayer = AI}) w returnScreen
                                                        | c == 'w' || c == 'W' -> if (whitePlayer st) == AI then optionsLoop (st {whitePlayer = Human}) w returnScreen
                                                                                  else optionsLoop (st {whitePlayer = AI}) w returnScreen
                                                        | c == 's' || c == 'S' -> do new_size <- getNewSize w
                                                                                     if new_size == Nothing then optionsLoop st w returnScreen
                                                                                     else if 8 <= fromJust new_size && fromJust new_size <= 26 then
                                                                                         if fromJust new_size /= (size (board st)) then startGame (st {board = ((board st) {size = fromJust new_size}), turn = Black}) w
                                                                                         else do updateWindow w $ do moveCursor 18 0
                                                                                                                     clearLine
                                                                                                                     drawString ("error: the size is already " ++ (show (fromJust new_size)))
                                                                                                 render
                                                                                                 loop
                                                                                     else do updateWindow w $ do moveCursor 18 0
                                                                                                                 clearLine
                                                                                                                 drawString "error: size must be an integer between 8 and 26 (inclusive)"
                                                                                             render
                                                                                             loop
                                                        | c == 'g' || c == 'G' -> if (gameMode st) == Othello then startGame (st {gameMode = Reversi, turn = Black}) w
                                                                                  else startGame (st {gameMode = Othello, turn = Black}) w
                                                        | c == 'h' || c == 'H' -> if (hintsToggle st) == On then optionsLoop (st {hintsToggle = Off}) w returnScreen
                                                                                  else optionsLoop (st {hintsToggle = On}) w returnScreen
                                                        | c == '\ESC' -> returnScreen st w
                                                        | c == 'r' || c == 'R' -> startGame (st {turn = Black}) w
                                                        | c == 'q' || c == 'Q' -> return ()
                                                      Just _ -> loop

getNewSize :: Window -> Curses (Maybe Int)
getNewSize w = do updateWindow w $ do moveCursor 18 0
                                      clearLine
                                      drawString "size: "
                  render
                  loop "" where
      loop input = do ev <- getEvent w Nothing
                      case ev of
                           Nothing -> loop input
                           Just (EventCharacter c)
                             | c == '\ESC' -> return Nothing
                             | c == '\n' -> if input /= "" then return (Just (read input :: Int))
                                            else return Nothing
                             | isDigit c -> do updateWindow w (drawString [c])
                                               render
                                               loop (input ++ [c])
                             | otherwise -> loop input
                           Just (EventSpecialKey KeyBackspace)
                             | length input > 0 -> do updateWindow w (drawString "\b \b")
                                                      render
                                                      loop (init input)
                             | otherwise -> loop ""
                           Just _ -> loop input

getPlayerType    :: GameState -> PlayerType
getPlayerType st | turn st == Black = blackPlayer st
                 | turn st == White = whitePlayer st

main :: IO ()
main = runCurses $ do setEcho False
                      w <- defaultWindow
                      setCursorMode CursorInvisible
                      arguments <- liftIO getArgs
                      if length arguments == 0 then mainMenu initGameState w
                      else do case checkArgs arguments of
                                  Nothing -> do updateWindow w $ do drawString "Invalid Entry -- See below for usage\n"
                                                                    drawString "Usage: ./Main <BlackPlayerType> (\"AI\" || \"Human\") <WhitePlayerType> (\"AI\" || \"Human\") <Size> (8..26) <GameMode> (\"Othello\" || \"Reversi\") <HintsToggle> (\"On\" || \"Off\")"
                                                render
                                  Just st -> mainMenu st w

mainMenu :: GameState -> Window -> Curses ()
mainMenu st w = do updateWindow w $ do clear
                                       moveCursor 0 0
                                       drawString "Othello, the videogame!\n"
                                       drawString "Start: enter\n"
                                       drawString "Options: escape\n"
                                       drawString "Quit: q\n"
                   render
                   loop where
                        loop = do ev <- getEvent w Nothing
                                  case ev of
                                       Nothing -> loop
                                       Just (EventCharacter c) | c == '\n' -> startGame st w
                                                               | c == '\ESC' -> optionsLoop st w (mainMenu)
                                                               | c == 'q' -> return ()
                                                               | c == 'Q' -> return ()
                                       Just _ -> loop

checkArgs :: [[Char]] -> Maybe (GameState)
checkArgs arguments = do if length arguments /= 5 then Nothing
                         else do let argPlayerBlack = checkPlayerType (arguments !! 0)
                                 let argPlayerWhite = checkPlayerType (arguments !! 1)
                                 let argSize = checkBoardSize (arguments !! 2)
                                 let argGameMode = checkGameMode (arguments !! 3)
                                 let argHintsToggle = checkHintsToggle (arguments !! 4)
                                 if trace' (argPlayerBlack) /= PLayerTypeError && trace' (argPlayerWhite) /= PLayerTypeError
                                         then if trace' (argSize) /= -1
                                                  then if trace' (argGameMode) /= GameModeError
                                                          then if trace' (argHintsToggle) /= HintsToggleError
                                                                  then Just (GameState (Board argSize 0 []) Black argPlayerBlack argPlayerWhite argGameMode argHintsToggle)
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

startGame :: GameState -> Window -> Curses ()
startGame st w | gameMode st == Othello = do let midPoint = (size (board st)) `div` 2
                                             let midPointLess = midPoint - 1
                                             let gameBoard = Board (size (board st)) 0 [((midPointLess, midPointLess), Black), ((midPointLess, midPoint), White), ((midPoint, midPointLess), White), ((midPoint, midPoint), Black)]
                                             gameLoop (st {board = gameBoard, turn = Black}) w
               | otherwise = startReversi (st {board = Board (size (board st)) 0 [], turn = Black}) w

startReversi                                :: GameState -> Window -> Curses ()
startReversi st w | getPlayerType st == Human = do updateWindow w $ do clear
                                                                       moveCursor 0 0
                                                                       drawString (showGameState st)
                                                                       drawString ((show (turn st)) ++ "'s turn (" ++ (getPieceStr (turn st)) ++ ")\n")
                                                                       drawString ("move: ")
                                                   render
                                                   move <- getMove st w getBestReversiInitialMove
                                                   if move == Quit then return ()
                                                   else if move == Options then optionsLoop st w startReversi
                                                   else do let (x, y) = getCoord ((\(Move coordinateString) -> coordinateString) move)
                                                           if x == -1 then do updateWindow w $ do moveCursor (toInteger (size (board st)) + 2) 0
                                                                                                  clearLine
                                                                                                  drawString "That is an invalid coordinate, please check your input and try again"
                                                                              render
                                                                              startReversi st w
                                                           else do let new_board = makeReversiInitialMove (board st) (turn st) (x, y)
                                                                   if isNothing new_board then do updateWindow w $ do moveCursor (toInteger (size (board st)) + 2) 0
                                                                                                                      clearLine
                                                                                                                      drawString "That is an invalid move, in Reversi, the first 4 moves must be within the center 2x2 square. Please try another"
                                                                                                  render
                                                                                                  startReversi st w
                                                                   else if length (pieces (fromJust new_board)) == 4 then gameLoop (st {board = fromJust new_board, turn = White}) w
                                                                   else
                                                                       if turn st == Black then startReversi (st {board = fromJust new_board, turn = White}) w
                                                                       else startReversi (st {board = fromJust new_board, turn = Black}) w
                  | otherwise = do let move = getBestReversiInitialMove (board st) (turn st)
                                   let new_board = fromJust (makeReversiInitialMove (board st) (turn st) move)
                                   if length (pieces new_board) == 4 then
                                      if turn st == Black then gameLoop (st {board = new_board, turn = White}) w
                                      else gameLoop (st {board = new_board, turn = Black}) w
                                   else
                                      if turn st == Black then startReversi (st {board = new_board, turn = White}) w
                                      else startReversi (st {board = new_board, turn = Black}) w



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
