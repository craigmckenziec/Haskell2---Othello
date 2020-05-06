{-|
Module : Main
Description : Handles operations directly concerning the main operations of the program, ones that are central to the programs operation
-}
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

-- | Time to display AI board for
boardDisplayTime :: Float -- ^ Time before triggers
boardDisplayTime = 1.5-- time that the results of moves are shown for (seconds)

-- | Time to wait before hint triggers
hintWaitTime :: Integer -- ^ Time before triggers
hintWaitTime = 30-- time delay before hint is given (seconds)

-- | Time to wait till turn timout (in addition to hint time)
turnTimeout :: Integer -- ^ Time before triggers
turnTimeout = 30

-- | Potential actions that can be taken by the user in the Move segment
data Action =  Options | Pass | Undo | Quit | Save | TimeOut | Move String
  deriving Eq

-- | represnets the turn loop, that recognises both what player's turn it is and what type that player is, and then directs the program to the proper path
gameLoop :: GameState -- ^ Current GameState
        -> Window -- ^ Current Window being displayed
        -> Curses () -- ^ Wrapper for curses that returns to main when complete
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

-- | Prints the game over screen to the user, including who won (or if it was a draw) and then returns to the main menu
gameOverScreen :: GameState -- ^ Current GameState
                -> Window -- ^ Current Window being displayed
                -> Curses () -- ^ Wrapper for curses that returns to main when complete
gameOverScreen st w = do let amountWhite = evaluateBoard (board st) White
                         let amountBlack = evaluateBoard (board st) Black
                         updateWindow w $ do clear
                                             moveCursor 0 0
                                             if (amountBlack > amountWhite) then drawString "Black Wins!"
                                             else if (amountWhite > amountBlack) then drawString "White Wins!"
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

-- | The game loop for the one depth AI player
aiGameLoop :: GameState -- ^ Current GameState
        -> Window -- ^ Current Window being displayed
        -> Curses () -- ^ Wrapper for curses that returns to main when complete
aiGameLoop st w = do if (length (getPossible (turn st) (board st)) == 0)
                            then do let currentBoard = board st
                                    let newState = st {board = Board (size currentBoard)  ((passes currentBoard) + 1) (pieces currentBoard), turn = (other (turn st))}
                                    gameLoop newState w
                            else do drawGameState st w
                                    action <- pause st w gameLoop boardDisplayTime
                                    if action == Options then optionsLoop st w gameLoop
                                    else do let move = getBestMoveOneDepth (board st) (turn st)
                                            let new_board = makeMove (board st) (turn st) (move)
                                            let newState =  st {board = fromJust new_board, turn = (other (turn st)), previousBoards = ((previousBoards st) ++ [(board st)])}
                                            gameLoop newState w

-- | The gameloop for a human player that recognises input using the getMove function to process the users input and move to the correct next phase
humanGameLoop :: GameState -- ^ Current GameState
                -> Window -- ^ Current Window being displayed
                -> Curses () -- ^ Wrapper for curses that returns to main when complete
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
                                        if ((gameMode st) == Reversi) && (length (pieces (board st)) <= 4) then startReversi newState w
                                        else gameLoop newState w
                          else if move == TimeOut 
                              then do let currentBoard = board st
                                      let newState = st {board = Board (size currentBoard)  ((passes currentBoard) + 1) (pieces currentBoard), turn = (other (turn st))}
                                      invalidMoveScreen st w ("You were automatically passed because you took too long! You only have " ++ show(turnTimeout + hintWaitTime) ++" seconds a turn!") humanGameLoop
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


-- | Used to retrieve a users move given by the user via waiting for an input event, and calling for a hint to be given and a timeout timer to begin if the user takes too long
getMove :: GameState -- ^ Current GameState
        -> Window -- ^ Current window being displayed
        -> (Board -> Col -> Position) -- ^ Function used to get the the best move according to the AI (for use by the hint generator)
        -> Curses Action -- ^ The action which is returned from the users input
getMove st w getBestMove = loop "" where
    loop input = do ev <- getEvent w (Just (hintWaitTime*10^3))
                    case ev of
                         Nothing -> do giveHint st w input getBestMove
                                       getMoveStartTimer st w input
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


-- | Same function as the getMove function, but now based around the timer for the timeout rather than the hint timer
getMoveStartTimer :: GameState-- ^ Current GameState
        -> Window -- ^ Current window being displayed
        -> String -- ^ Input given by the user so far
        -> Curses Action -- ^ The action which is returned from the users input
getMoveStartTimer st w input = loop input where
    loop input = do ev <- getEvent w (Just (turnTimeout*10^3))
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
                   
-- | Used to pause the game so that the user can see AI move more clearly
pause :: GameState -- ^ Current GameState
        -> Window -- ^ Current window being displayed
        -> (GameState -> Window -> Curses ()) -- ^ Function representing the screen that should be returned to if necessary 
        -> Float -- ^ Float representing the time to wait while the pause lasts
        -> Curses Action -- ^ Curses ACtion that should be returned depending on user input
pause st w returnScreen waitTime = loop where
    loop = do ev <- getEvent w (Just (round(waitTime*10^3)))
              case ev of
                   Nothing -> return Pass
                   Just (EventCharacter '\ESC') -> return Options
                   Just _ -> loop

-- | Turns a given GameState back to to the previous human move
undoMove :: GameState -- ^ The current GameState
        -> GameState -- ^ The reverted GameState
undoMove (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards)
                    = do if (turn == Black)
                            then if (whitePlayer == Human)
                                      then undoMoveHuman (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards)
                                      else undoMoveAI (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards)
                            else if (blackPlayer == Human)
                                      then undoMoveHuman (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards)
                                      else undoMoveAI (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards)


-- | Undo back to the previous move (if the opponent is a human)
undoMoveHuman :: GameState -- ^ The current GameState
                -> GameState -- ^ The reverted GameState
undoMoveHuman (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards)
                       = do let lastMove = last previousBoards
                            let firstMoves = init previousBoards
                            GameState lastMove (other turn) blackPlayer whitePlayer gameMode hintsToggle firstMoves
                      
-- | Undo back two moves (if the opponent is an AI)                      
undoMoveAI :: GameState -- ^ The current GameState
                -> GameState -- ^ The reverted GameState
undoMoveAI (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards)
                      = do let firstMoves = init previousBoards
                           let lastHumanMove = last firstMoves
                           let previousMoves = init firstMoves
                           GameState lastHumanMove turn blackPlayer whitePlayer gameMode hintsToggle previousMoves

-- | Gives a hint to the user by finding the best current move (according to the AI)
giveHint :: GameState -- ^ The current GameState
        -> Window -- ^ The current window being displayed
        -> String -- ^ The input that the user has already given that needs to be rewritten so it is not erased
        -> (Board -> Col -> Position) -- ^ Function used to get the the best move according to the AI (for use by the hint generator)
        -> Curses () -- ^ Curses wrapper that will be passed back to main upon completion
giveHint st w input getBestMove | hintsToggle st == On = do let (x, y) = getBestMove (board st) (turn st)
                                                            updateWindow w $ do moveCursor (toInteger (getYCoord (size (board st)))) 0
                                                                                clearLine
                                                                                drawString ((show (turn st)) ++ "'s turn\n")
                                                                                drawString ("Hint: " ++ [toEnum (65+x)::Char] ++ (show (y)))
                                                                                drawString ("\nWarning! You only have " ++ show(turnTimeout) ++ " seconds before you automatically pass on your turn!")
                                                                                drawString "\nmove: "
                                                            render
                                                            when (length input > 0) (do updateWindow w (drawString input)
                                                                                        render)
                                | otherwise = return ()


-- | Displays the potential options to the user and allows them to change ones they see fit
optionsLoop :: GameState -- ^ Current GameState
                -> Window -- ^ Current window being displayed
                -> (GameState -> Window -> Curses ()) -- ^ The function that the screen should return to (allows the options loop to return to where it was called from)
                -> Curses () -- ^ Curses wrapper that will be passed back to main upon completion
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

-- | Dispalyed when an invalid move or a move that does is simply there to provide a message to the user
invalidMoveScreen :: GameState -- ^ Current GameState
                -> Window -- ^ Current window being displayed
                -> String -- ^ The message that should be displayed to the user
                -> (GameState -> Window -> Curses ()) -- ^ The function that the screen should return to (allows the options loop to return to where it was called from)
                -> Curses () -- ^ Curses wrapper that will be passed back to main upon completion
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

-- | Used to return a new size for the board
getNewSize :: Window -- ^ Window currently being displayed
        -> Curses (Maybe Int) -- ^ Maybe Int that returns a new size if the size is valid or Nothing if they choose to not change the size
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

-- | Returns a PlayerType object dependant on what colour is passed in 
getPlayerType    :: GameState -- ^ Current GameState
                -> PlayerType -- ^ PlayerType dependant on what colour is present
getPlayerType st | turn st == Black = blackPlayer st
                 | turn st == White = whitePlayer st

-- | Main method which is used to start the curses window and recieve the arguments from the commandline
main :: IO () -- ^ Overarching IO wrapper of the program
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

-- | Used to check whether or not a given savefiles contents are valid or not
checkGameState :: [String] -- ^ List of Strings representing the instructions read in  
                -> Window -- ^ Current window being displayed
                -> Curses () -- ^ Curses wrapper which chains the program back to main
checkGameState fileContents w = do case buildGameState fileContents of
                                        Nothing -> do updateWindow w $ do drawString "Your Save game is broken, please regenerate and try again!\n"
                                                      render
                                                      loop where
                                                                   loop = do ev <- getEvent w Nothing
                                                                             case ev of
                                                                                     Nothing -> loop
                                                                                     Just _ -> return ()
                                        Just st -> gameLoop st w

-- | Attempts to build the Gamestate described in the save file and returns depending whether or not the file is valid
buildGameState :: [String] -- ^ List of Strings representing a given save file
                -> Maybe (GameState) -- ^ Gamestate is returned representing the savefile if its contents were valid or Nothing is the file was invalid
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

-- | Takes in a list of pieces and returns only the positions within the list
getPositions :: [(Position, Col)] -- ^ A list of pieces
                -> [Position] -- ^ Only the list of positions with the colours stripped away
getPositions [] = []
getPositions (((x,y),colour):qs) = [(x,y)] ++ getPositions qs

-- | Tells you whether or not a list of positions has a duplicate or not
hasDuplicates :: [Position] -- ^ List of positions to be checked
                -> Bool -- ^ True if all are unique, false if not
hasDuplicates ts = allUnique ts
 

-- | Checks whether or not the last piece has the -1 trigger representing a failed Pieces build
validateLastPiece :: (Position, Col) -- ^ List of pieces to be checked
                        -> Int -- ^ Integer representing whether or not the build was successful (1 if it was, 0 if it wasn't)
validateLastPiece ((x,y),colour) = if (x == -1 || y == -1) 
                                        then 0
                                        else 1

-- | Check the save games pieces for whether or not they are valid
checkPieces :: [String] -- ^ List of pieces represented as strings
                -> Int -- ^ Board size
                -> [(Position,Col)] -- ^ List of pieces
checkPieces [] size = []
checkPieces (q:qs) size = do case checkPiece q size of
                                Nothing -> do let position = (-1,-1)
                                              [(position,White)]
                                Just piece -> [piece] ++ checkPieces qs size
                         

-- | Check a given string for whether or not it represnts a valid piece
checkPiece :: String -- ^ String representing potential piece
                -> Int -- ^ Board size
                -> Maybe (Position, Col) -- ^ Nothing if the String was invalid or a Piece object if the String was valid 
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
                      

-- | Calls the game's main menu and displays to the user the options at this point, and then waits for input
mainMenu :: GameState -- ^ Current GameState
        -> Window -- ^ Current Window being displayed
        -> Curses () -- ^ Wrapper for curses that returns to main when complete
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

-- | Checks whether the arguments passed in by the user are valid or not
checkArgs :: [[Char]] -- ^ List of a list of Chars that represents the arguements passed in from the command line
        -> Maybe (GameState) -- ^ Gamestate object if the list of arguments was valid or Nothing if the input was invalid
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

-- | Check whether a given String representing a PlayerType is valid or not
checkPlayerType :: String -- ^ String representing potential PlayerType
                -> PlayerType -- ^ PlayerType object of either valid type or error
checkPlayerType player = if player == "AI"
                                then AI
                         else if player == "Human"
                                then Human
                                else PLayerTypeError

-- | Check whether a given String representing a Board size is valid or not
checkBoardSize :: String -- ^ String representing potential board size
                -> Int -- ^ Integer representing a board size of either valid type or error
checkBoardSize size = if checkDigits size == True
                            then do let sizeInt = (read size :: Int)
                                    if sizeInt < 27 && sizeInt > 7
                                         then sizeInt
                                         else -1
                            else -1

-- | Check whether a given String representing a coordinate is valid or not
checkCoord :: String -- ^ String representing potential coord
                -> Int -- ^ Board size
                -> Int -- ^ Integer representing a coord of either valid type or error
checkCoord position size = if checkDigits position == True
                                     then do let positionInt = (read position :: Int)
                                             if positionInt < size && positionInt > (-1)
                                                     then positionInt
                                                     else -1
                                     else -1

-- | Check whether a given String representing a GameMode is valid or not
checkGameMode :: String -- ^ String representing potential GameMode
                -> GameMode -- ^ GameMode object of either valid type or error
checkGameMode gameMode = if gameMode == "Othello"
                            then Othello
                         else if gameMode == "Reversi"
                            then Reversi
                         else GameModeError

-- | Check whether a given String representing the HintsToggle is valid or not
checkHintsToggle :: String -- ^ String representing potential HintsToggle
                -> HintsToggle -- ^ HintsToggle object of either valid type or error
checkHintsToggle toggle = if toggle == "On"
                             then On
                          else if toggle == "Off"
                             then Off
                          else HintsToggleError

-- | Check whether a given String representing a PlayerTurn is valid or not
checkPlayerTurn :: String -- ^ String representing potential turn
                -> Col -- ^ Col object of either valid type or error
checkPlayerTurn colour = if colour == "Black"
                             then Black
                         else if colour == "White"
                             then White
                         else ColourError

-- | Check whether a given String representing a PlayerType is valid or not
checkPasses :: String -- ^ String representing potential passes
                -> Int -- ^ Integer representing the passes of either valid type or error
checkPasses passes = if checkDigits passes == True
                            then do let sizeInt = (read passes :: Int)
                                    if sizeInt < 2 && sizeInt > (-1)
                                         then sizeInt
                                         else -1
                            else -1


-- | Called to start a new game of either the othello or reversi varieties
startGame :: GameState -- ^ Current GameState
        -> Window -- ^ Current Window being displayed
        -> Curses () -- ^ Wrapper for curses that returns to main when complete
startGame st w | gameMode st == Othello = do let midPoint = (size (board st)) `div` 2
                                             let midPointLess = midPoint - 1
                                             let gameBoard = Board (size (board st)) 0 [((midPointLess, midPointLess), Black), ((midPointLess, midPoint), White), ((midPoint, midPointLess), White), ((midPoint, midPoint), Black)]
                                             gameLoop (st {board = gameBoard, turn = Black, previousBoards = []}) w
               | otherwise = startReversi (st {board = Board (size (board st)) 0 [], turn = Black, previousBoards = []}) w

-- | Starts a reversi loops and loops through, allowing both sides to pick their chosen starting pieces (with valiation for these choices)
startReversi :: GameState -- ^ Current GameState
                -> Window -- ^ Current Window being displayed
                -> Curses () -- ^ Wrapper for curses that returns to main when complete
startReversi st w | getPlayerType st == Human = do drawGameState st w
                                                   move <- getMove st w getBestReversiInitialMove
                                                   if move == Quit then return ()
                                                   else if move == Pass then do let currentBoard = board st
                                                                                let newState = st {board = Board (size currentBoard)  ((passes currentBoard) + 1) (pieces currentBoard), turn = (other (turn st))}
                                                                                startReversi newState w
                                                   else if move == Options then optionsLoop st w startReversi
                                                   else if move == Save
                                                       then do result <- liftIO (saveGame st)
                                                               invalidMoveScreen st w "Game Saved" startReversi
                                                   else if move == Undo
                                                       then if (length (previousBoards st) == 0)
                                                         then invalidMoveScreen st w "You can't undo on the first move!" startReversi
                                                         else do let newState = undoMove st
                                                                 startReversi newState w
                                                   else if move == TimeOut
                                                       then do let currentBoard = board st
                                                               let newState = st {board = Board (size currentBoard)  ((passes currentBoard) + 1) (pieces currentBoard), turn = (other (turn st))}
                                                               invalidMoveScreen st w ("You were automatically passed because you took too long! You only have " ++ show(turnTimeout + hintWaitTime) ++" seconds a turn!") startReversi
                                                   else do let (x, y) = getCoord ((\(Move coordinateString) -> coordinateString) move)
                                                           if x == -1 then invalidMoveScreen st w "That is an invalid coordinate, please check your input and try again" startReversi
                                                           else do let new_board = makeReversiInitialMove (board st) (turn st) (x, y)
                                                                   if isNothing new_board then invalidMoveScreen st w "That is an invalid move, in Reversi, the first 4 moves must be within the center 2x2 square. Please try another" startReversi
                                                                   else if length (pieces (fromJust new_board)) == 4 then gameLoop (st {board = fromJust new_board, turn = (other (turn st)), previousBoards = ((previousBoards st) ++ [(board st)])}) w
                                                                   else startReversi (st {board = fromJust new_board, turn = (other (turn st)), previousBoards = ((previousBoards st) ++ [(board st)])}) w
                  | otherwise = do let move = getBestReversiInitialMove (board st) (turn st)
                                   let new_board = fromJust (makeReversiInitialMove (board st) (turn st) move)
                                   if length (pieces new_board) == 4 then  gameLoop (st {board = new_board, turn = (other (turn st)), previousBoards = ((previousBoards st) ++ [(board st)])}) w
                                   else startReversi (st {board = new_board, turn = (other (turn st)), previousBoards = ((previousBoards st) ++ [(board st)])}) w

