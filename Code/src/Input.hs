module Input where

import Board
import AI
import Data.Char (isDigit)
import Data.Char(digitToInt)
import System.IO
import UI.NCurses
import Control.Monad




-- Given an input string, convert it to a board coordinate as a pair of Ints
-- e.g. getCoord "D4" => (3,3) since coordinates are 0-based internally.
--   or getCoord "F2" => (5,1)
getCoord :: String -> (Int, Int)
getCoord move = if length(move) > 1
                then do let asciiValue = fromEnum (head move)
                        let reducedValue = asciiValue-65
                        if reducedValue < 0 || reducedValue > 25
                            then (-1, 0)
                            else
                                do let secondValue = drop 1 move
                                   if checkDigits secondValue == True
                                        then do let secondValueInt = (read secondValue :: Int)
                                                if reducedValue < 26 && secondValueInt < 26
                                                   then (reducedValue, secondValueInt)
                                                   else (-1, 0)
                                        else (-1, 0)
                else (-1, 0)



checkDigits :: [Char] -> Bool
checkDigits [] = True
checkDigits (x:xs) = do if isDigit x == True then checkDigits xs
                        else False

saveGame :: GameState -> IO ()
saveGame st = do let contents = createSaveGame st
                 writeFile "test.txt" contents

saveGameTest :: GameState -> IO ()
saveGameTest st = do let contents = createSaveGame st
                     writeFile "bitch.txt" contents

createSaveGame :: GameState -> String
createSaveGame (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards) 
                  = do let turnString = buildColour turn
                       let blackPlayerString = buildPlayer blackPlayer
                       let whitePlayerString = buildPlayer whitePlayer
                       let gameModeString = buildGameMode gameMode
                       let hintsToggleString = buildHints hintsToggle
                       let boardString = buildBoard board
                       (turnString ++ blackPlayerString ++ whitePlayerString ++ gameModeString  ++ hintsToggleString ++ boardString)
                       

buildColour :: Col -> String
buildColour turn = if (turn == White)
                    then "White\n"
                    else "Black\n"

buildPlayer :: PlayerType -> String
buildPlayer player = if (player == AI)
                        then "AI\n"
                        else "Human\n"

buildGameMode :: GameMode -> String
buildGameMode gameMode = if (gameMode == Othello)
                            then "Othello\n"
                            else "Reversi\n"

buildHints :: HintsToggle -> String
buildHints hintsToggle = if (hintsToggle == On)
                            then "On\n"
                            else "Off\n"

buildBoard :: Board -> String
buildBoard (Board size passes pieces) 
                = do let sizeString = buildInt size
                     let passesString = buildInt passes
                     let piecesString = buildPieces pieces
                     (sizeString ++ passesString ++ piecesString)

buildInt :: Int -> String
buildInt integer = show integer ++ "\n"

buildPieces :: [(Position, Col)] -> String
buildPieces [] = ""
buildPieces (((x,y), colour): qs) 
                = do let positionString = (show x) ++ "," ++ (show y)
                     let colourString = buildColour colour
                     positionString ++ "/" ++ colourString ++ (buildPieces qs)
    




