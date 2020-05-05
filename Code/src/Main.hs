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
import Data.List.Unique
import Data.Maybe
import Debug.Trace
import System.Environment
import System.IO
import Text.Read
import UI.NCurses
import System.IO.Error

hintWaitTime :: Integer
hintWaitTime = 3-- time delay before hint is given (seconds)

turnTimout :: Integer
turnTimout = 30

data Action =  Options | Pass | Undo | Quit | Save | TimeOut | Move String
  deriving Eq

gameLoop :: GameState -> Window -> Curses ()
gameLoop st w
    = do if gameOver (board st) == True
            then gameOverScreen st w
            else do if (turn st) == Black
                        then if blackPlayer st == Human
                            then humanGameLoop st w
                            else aiGameLoop st w
                    else if whitePlayer st == Human
                            then humanGameLoop st w
                            else aiGameLoop st w

gameOverScreen :: GameState -> Window -> Curses ()
gameOverScreen st w = do let ammountWhite = evaluateBoard (board st) White
                         let ammountBlack = evaluateBoard (board st) Black
                         updateWindow w $ do clear
                                             moveCursor 0 0
                                             if (ammountBlack > ammountWhite) then drawString "Black Wins!"
                                             else if (ammountWhite > ammountBlack) then drawString "White Wins!"
                                             else drawString "Draw!"
                         updateWindow w $ do moveCursor 1 0
                                             drawString "press enter to return to the menu"
                         render
                         loop where
  loop = do ev <- getEvent w Nothing
            case ev of
                 Nothing -> loop
                 Just (EventCharacter '\n') -> mainMenu st w
                 Just _ -> loop

aiGameLoop :: GameState -> Window -> Curses ()
aiGameLoop st w = do if (length (getPossible (turn st) (board st)) == 0)
                            then do let currentBoard = board st
                                    let newState = st {board = Board (size currentBoard)  ((passes currentBoard) + 1) (pieces currentBoard), turn = (other (turn st))}
                                    gameLoop newState w
                            else do let move = getBestMoveOneDepth (board st) (turn st)
                                    let new_board = makeMove (board st) (turn st) (move)
                                    if turn st == Black then gameLoop st {board = fromJust new_board, turn = White, previousBoards = ((previousBoards st) ++ [(board st)])} w
                                                        else gameLoop st {board = fromJust new_board, turn = Black, previousBoards = ((previousBoards st) ++ [(board st)])} w

humanGameLoop :: GameState -> Window -> Curses ()
humanGameLoop st w = do drawGameState st w
                        move <- getMove st w getBestMoveOneDepth
                        if move == Quit then return ()
                          else if move == Pass
                              then do let currentBoard = board st
                                      let newState = st {board = Board (size currentBoard)  ((passes currentBoard) + 1) (pieces currentBoard), turn = (other (turn st))}
                                      gameLoop newState w
                          else if move == Options
                              then optionsLoop st w gameLoop
                          else if move == Save
                              then do result <- liftIO (saveGame st)
                                      invalidMoveScreen st w "Game Saved" humanGameLoop
                          else if move == Undo
                              then if (length (previousBoards st) == 0)
                                then invalidMoveScreen st w "You can't undo on the first move!" humanGameLoop
                                else do let newState = undoMove st
                                        gameLoop newState w
                          else if move == TimeOut 
                              then do let currentBoard = board st
                                      let newState = st {board = Board (size currentBoard)  ((passes currentBoard) + 1) (pieces currentBoard), turn = (other (turn st))}
                                      timeoutMoveScreen newState w gameLoop
                          else  do
                                  let (x, y) = getCoord ((\(Move coordinateString) -> coordinateString) move)
                                  if x == -1
                                      then invalidMoveScreen st w "That is an invalid coordinate, please check your input and try again" humanGameLoop
                                      else
                                          do  let new_board = makeMove (board st) (turn st) (x, y)
                                              if isNothing new_board
                                                  then invalidMoveScreen st w "That is an invalid move, please try another" humanGameLoop
                                              else
                                                  if turn st == Black then gameLoop (st {board = fromJust new_board, turn = White, previousBoards = ((previousBoards st) ++ [(board st)])}) w
                                                  else gameLoop (st {board = fromJust new_board, turn = Black, previousBoards = ((previousBoards st) ++ [(board st)])}) w


getMove :: GameState -> Window -> (Board -> Col -> Position) -> Curses Action
getMove st w getBestMove = loop "" where
    loop input = do ev <- getEvent w (Just (hintWaitTime*10^3))
                    case ev of
                         Nothing -> do giveHint st w input getBestMove
                                       getMoveStartTimer st w
                         Just (EventSpecialKey KeyBackspace)
                           | length input > 0 -> do updateWindow w (drawString "\b \b")
                                                    render
                                                    loop (init input)
                           | otherwise -> loop ""
                         Just (EventCharacter c)
                           | c == '\ESC' -> return Options
                           | c == '\n' -> return (Move input)
                           | c == '-' -> return Undo
                           | c == '+' -> return Pass
                           | c == '>' -> return Save
                           | isAlphaNum c -> do updateWindow w (drawString [c])
                                                render
                                                loop (input ++ [c])
                           | otherwise -> loop input
                         Just _ -> loop input

getMoveStartTimer :: GameState -> Window -> Curses Action
getMoveStartTimer st w = loop "" where
    loop input = do ev <- getEvent w (Just (turnTimout*10^3))
                    case ev of
                         Nothing -> return TimeOut
                
                         Just (EventSpecialKey KeyBackspace)
                           | length input > 0 -> do updateWindow w (drawString "\b \b")
                                                    render
                                                    loop (init input)
                           | otherwise -> loop ""
                         Just (EventCharacter c)
                           | c == '\ESC' -> return Options
                           | c == '\n' -> return (Move input)
                           | c == '-' -> return Undo
                           | c == '+' -> return Pass
                           | c == '>' -> return Save
                           | isAlphaNum c -> do updateWindow w (drawString [c])
                                                render
                                                loop (input ++ [c])
                           | otherwise -> loop input
                         Just _ -> loop input
                   


undoMove :: GameState -> GameState
undoMove (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards)
                    = do if (turn == Black)
                            then if (whitePlayer == Human)
                                      then undoMoveHuman (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards)
                                      else undoMoveAI (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards)
                            else if (blackPlayer == Human)
                                      then undoMoveHuman (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards)
                                      else undoMoveAI (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards)



undoMoveHuman :: GameState -> GameState
undoMoveHuman (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards)
                       = do let lastMove = last previousBoards
                            let firstMoves = init previousBoards
                            GameState lastMove (other turn) blackPlayer whitePlayer gameMode hintsToggle firstMoves
                      
undoMoveAI :: GameState -> GameState
undoMoveAI (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards)
                      = do let firstMoves = init previousBoards
                           let lastHumanMove = last firstMoves
                           let previousMoves = init firstMoves
                           GameState lastHumanMove turn blackPlayer whitePlayer gameMode hintsToggle previousMoves

giveHint :: GameState -> Window -> String -> (Board -> Col -> Position) -> Curses ()
giveHint st w input getBestMove | hintsToggle st == On = do let (x, y) = getBestMove (board st) (turn st)
                                                            updateWindow w $ do moveCursor (toInteger (getYCoord (size (board st)))) 0
                                                                                clearLine
                                                                                drawString ((show (turn st)) ++ "'s turn\n")
                                                                                drawString ("Hint: " ++ [toEnum (65+x)::Char] ++ (show (y)))
                                                                                drawString ("\nWarning! You only have " ++ show(turnTimout) ++ " seconds before you automatically pass on your turn!")
                                                                                drawString "\nmove: "
                                                            render
                                                            when (length input > 0) (do updateWindow w (drawString input)
                                                                                        render)
                                | otherwise = return ()

optionsLoop      :: GameState -> Window -> (GameState -> Window -> Curses ()) -> Curses ()
optionsLoop st w returnScreen = do updateWindow w $ do clear
                                                       moveCursor 0 0
                                                       drawString "Options\n\n"
                                                       drawString ("Black player (AI/Human): " ++ show (blackPlayer st) ++ "\n")
                                                       drawString ("White player (AI/Human): " ++ show (whitePlayer st) ++ "\n")
                                                       drawString ("Size (8..26): " ++ show (size (board st)) ++ "\n")
                                                       drawString ("Game mode (Othello/Reversi): " ++ show (gameMode st) ++ "\n")
                                                       drawString ("Hints (On/Off): " ++ show (hintsToggle st) ++ "\n\n")
                                                       drawString "Change black player: b\n"
                                                       drawString "Change white player: w\n"
                                                       drawString "Change size: s\n"
                                                       drawString "Change game mode: g\n"
                                                       drawString "Toggle hints: h\n\n"
                                                       drawString "Resume: escape or enter\n"
                                                       drawString "Restart: r\n"
                                                       drawString "Quit: q\n"
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
                                                        | c == '\ESC' || c == '\n' -> returnScreen st w
                                                        | c == 'r' || c == 'R' -> startGame st w
                                                        | c == 'q' || c == 'Q' -> return ()
                                                      Just _ -> loop

invalidMoveScreen :: GameState -> Window -> String -> (GameState -> Window -> Curses ()) -> Curses ()
invalidMoveScreen st w message returnScreen = do updateWindow w $ do clear
                                                                     moveCursor 0 0
                                                                     drawString message
                                                                     drawString "\nPress enter to continue"
                                                 render
                                                 loop where
    loop = do ev <- getEvent w Nothing
              case ev of
                   Nothing -> loop
                   Just (EventCharacter '\n') -> returnScreen st w
                   Just _ -> loop

timeoutMoveScreen :: GameState -> Window -> (GameState -> Window -> Curses ()) -> Curses ()
timeoutMoveScreen st w returnScreen = do updateWindow w $ do clear
                                                             moveCursor 0 0
                                                             drawString "You were automatically passed because you took too long! You only have 30 seconds a turn!"
                                                             drawString "\nPress enter to continue"
                                         render
                                         loop where
    loop = do ev <- getEvent w Nothing
              case ev of
                   Nothing -> loop
                   Just (EventCharacter '\n') -> returnScreen st w
                   Just _ -> loop

getNewSize :: Window -> Curses (Maybe Int)
getNewSize w = do updateWindow w $ do moveCursor 18 0
                                      clearLine
                                      drawString "Size: "
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
                      else if length arguments == 1 
                              then do content <- liftIO (tryIOError $ readFile (arguments !! 0))
                                      case content of
                                         Left except -> do updateWindow w $ do drawString "Invalid Entry -- See below for usage (Please ensure the name of your file is correct)\n"
                                                                               drawString "Usage: ./Main <PathToSave>.txt"
                                                           render
                                                           loop where
                                                                   loop = do ev <- getEvent w Nothing
                                                                             case ev of
                                                                                     Nothing -> loop
                                                                                     Just _ -> return ()
                                         Right contents -> checkGameState (lines contents) w
                      else do case checkArgs arguments of
                                  Nothing -> do updateWindow w $ do drawString "Invalid Entry -- See below for usage\n"
                                                                    drawString "Usage: ./Main <BlackPlayerType> (\"AI\" || \"Human\") <WhitePlayerType> (\"AI\" || \"Human\") <Size> (8..26) <GameMode> (\"Othello\" || \"Reversi\") <HintsToggle> (\"On\" || \"Off\")"
                                                render
                                                loop where
                                                                   loop = do ev <- getEvent w Nothing
                                                                             case ev of
                                                                                     Nothing -> loop
                                                                                     Just _ -> return ()
                                  Just st -> mainMenu st w


checkGameState :: [String] -> Window -> Curses ()
checkGameState fileContents w = do case buildGameState fileContents of
                                        Nothing -> do updateWindow w $ do drawString "Your Save game is broken, please regenerate and try again!\n"
                                                      render
                                                      loop where
                                                                   loop = do ev <- getEvent w Nothing
                                                                             case ev of
                                                                                     Nothing -> loop
                                                                                     Just _ -> return ()
                                        Just st -> gameLoop st w

buildGameState :: [String] -> Maybe (GameState)
buildGameState fileContents = if (length fileContents) > 8
                                     then do let saveColour = checkPlayerTurn (fileContents !! 0)
                                             let savePlayerBlack = checkPlayerType (fileContents !! 1)
                                             let savePlayerWhite = checkPlayerType (fileContents !! 2)
                                             let saveGameMode = checkGameMode (fileContents !! 3)
                                             let saveHintsToggle = checkHintsToggle (fileContents !! 4)
                                             let saveSize = checkBoardSize (fileContents !! 5)
                                             let savePasses = checkPasses (fileContents !! 6)
                                             let savePieces = checkPieces (snd (splitAt 7 fileContents)) saveSize
                                             if (saveColour /= ColourError && savePlayerBlack /= PLayerTypeError && savePlayerWhite /= PLayerTypeError)
                                                     then if (saveGameMode /= GameModeError && saveHintsToggle /= HintsToggleError)
                                                             then if (saveSize /= (-1) && savePasses /= (-1))
                                                                     then if (validateLastPiece (last savePieces) == 1 && hasDuplicates (getPositions savePieces) == True)
                                                                                then do let sta = (GameState (Board saveSize savePasses savePieces) saveColour savePlayerBlack savePlayerWhite saveGameMode saveHintsToggle [])
                                                                                        Just sta
                                                                                else Nothing
                                                                     else Nothing
                                                             else Nothing
                                                     else Nothing
                                     else Nothing

getPositions :: [(Position, Col)] -> [Position]
getPositions [] = []
getPositions (((x,y),colour):qs) = [(x,y)] ++ getPositions qs

hasDuplicates :: [Position] -> Bool
hasDuplicates ts = allUnique ts
 

validateLastPiece :: (Position, Col) -> Int
validateLastPiece ((x,y),colour) = if (x == -1 || y == -1) 
                                        then 0
                                        else 1

checkPieces :: [String] -> Int -> [(Position,Col)]
checkPieces [] size = []
checkPieces (q:qs) size = do case checkPiece q size of
                                Nothing -> do let position = (-1,-1)
                                              [(position,White)]
                                Just piece -> [piece] ++ checkPieces qs size
                         
checkPiece :: String -> Int -> Maybe (Position, Col)
checkPiece piece size = do let breakPartsFirst = splitOn "/" piece
                           if (length breakPartsFirst == 2) 
                                   then do let colour = checkPlayerTurn (breakPartsFirst !! 1)
                                           let pieceDetails = splitOn "," (breakPartsFirst !! 0)
                                           if (length pieceDetails == 2 && colour /= ColourError)
                                                        then do let xpos = checkCoord (pieceDetails !! 0) size
                                                                let ypos = checkCoord (pieceDetails !! 1) size
                                                                if (xpos /= (-1) && ypos /= (-1))
                                                                        then do let position = (xpos, ypos)
                                                                                Just (position, colour)
                                                                        else Nothing
                                                        else Nothing
                                   else Nothing
                      

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
                                       Just (EventCharacter c)
                                         | c == '\n' -> startGame st w
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
                                                                  then Just (GameState (Board argSize 0 []) Black argPlayerBlack argPlayerWhite argGameMode argHintsToggle [])
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

checkCoord :: String -> Int -> Int
checkCoord position size = if checkDigits position == True
                                     then do let positionInt = (read position :: Int)
                                             if positionInt < size && positionInt > (-1)
                                                     then positionInt
                                                     else -1
                                     else -1

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

checkPlayerTurn :: String -> Col
checkPlayerTurn colour = if colour == "Black"
                             then Black
                         else if colour == "White"
                             then White
                         else ColourError
                
checkPasses :: String -> Int 
checkPasses passes = if checkDigits passes == True
                            then do let sizeInt = (read passes :: Int)
                                    if sizeInt < 2 && sizeInt > (-1)
                                         then sizeInt
                                         else -1
                            else -1


startGame :: GameState -> Window -> Curses ()
startGame st w | gameMode st == Othello = do let midPoint = (size (board st)) `div` 2
                                             let midPointLess = midPoint - 1
                                             let gameBoard = Board (size (board st)) 0 [((midPointLess, midPointLess), Black), ((midPointLess, midPoint), White), ((midPoint, midPointLess), White), ((midPoint, midPoint), Black)]
                                             gameLoop (st {board = gameBoard, turn = Black, previousBoards = []}) w
               | otherwise = startReversi (st {board = Board (size (board st)) 0 [], turn = Black, previousBoards = []}) w

startReversi                                :: GameState -> Window -> Curses ()
startReversi st w | getPlayerType st == Human = do drawGameState st w
                                                   move <- getMove st w getBestReversiInitialMove
                                                   if move == Quit then return ()
                                                   else if move == Options then optionsLoop st w startReversi
                                                   else do let (x, y) = getCoord ((\(Move coordinateString) -> coordinateString) move)
                                                           if x == -1 then invalidMoveScreen st w "That is an invalid coordinate, please check your input and try again" startReversi
                                                           else do let new_board = makeReversiInitialMove (board st) (turn st) (x, y)
                                                                   if isNothing new_board then invalidMoveScreen st w "That is an invalid move, in Reversi, the first 4 moves must be within the center 2x2 square. Please try another" startReversi
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

