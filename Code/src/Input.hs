{-|
Module : Input
Description : Handles operations directly concerning the input from the user and the output of files (such as saving)
-}
module Input where

import Board
import AI
import Data.Char (isDigit)
import Data.Char(digitToInt)
import System.IO
import UI.NCurses
import Control.Monad


-- | Given an input string, convert it to a board coordinate as a pair of Ints
-- e.g. getCoord "D4" => (3,3) since coordinates are 0-based internally.
--   or getCoord "F2" => (5,1)
getCoord :: String -- ^ String that is to be turned into a coordinate
          -> (Int, Int) -- ^ Pair of ints that either represent a 2-d coordinate or a failure to turn into coordinates resulting in (-1,0)
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


-- | Checks whether a given array of characters is made up entirely of integers or not
checkDigits :: [Char] -- ^ Array of characters to check
               -> Bool -- ^ True if all digits, false if otherwise
checkDigits [] = True
checkDigits (x:xs) = do if isDigit x == True then checkDigits xs
                        else False

-- | Operation which saves the the current gamestate to a save file
saveGame :: GameState -- ^ Current state of the game to be saved
          -> IO () -- ^ IO Wrapper to return the writeFile operation
saveGame st = do let contents = createSaveGame st
                 writeFile "savegame.txt" contents

-- | Takes a GameState and turns it into a String represting all the details of the state that will then be printed to the savegame file
createSaveGame :: GameState -- ^ Current GameState to be converted into a String
               -> String -- ^ String representing the GameState
createSaveGame (GameState board turn blackPlayer whitePlayer gameMode hintsToggle previousBoards) 
                  = do let turnString = buildColour turn
                       let blackPlayerString = buildPlayer blackPlayer
                       let whitePlayerString = buildPlayer whitePlayer
                       let gameModeString = buildGameMode gameMode
                       let hintsToggleString = buildHints hintsToggle
                       let boardString = buildBoard board
                       (turnString ++ blackPlayerString ++ whitePlayerString ++ gameModeString  ++ hintsToggleString ++ boardString)
                       
-- | Build a given colour into a String
buildColour :: Col -- ^ Colour to be converted
          -> String -- ^ Converted String
buildColour turn = if (turn == White)
                    then "White\n"
                    else "Black\n"

-- | Build a given PlayerType into a String
buildPlayer :: PlayerType -- ^ PlayerType to be converted
               -> String -- ^ Converted String
buildPlayer player = if (player == AI)
                        then "AI\n"
                        else "Human\n"

-- | Build a given GameMode into a String
buildGameMode :: GameMode -- ^ GameMode to be converted
               -> String -- ^ Converted String
buildGameMode gameMode = if (gameMode == Othello)
                            then "Othello\n"
                            else "Reversi\n"

-- | Build a given HintsToggle into a String
buildHints :: HintsToggle -- ^ HintsToggle to be converted
               -> String -- ^ Converted String
buildHints hintsToggle = if (hintsToggle == On)
                            then "On\n"
                            else "Off\n"

-- | Build a given Board into a String
buildBoard :: Board -- ^ Board to be converted
          -> String -- ^ Converted String
buildBoard (Board size passes pieces) 
                = do let sizeString = buildInt size
                     let passesString = buildInt passes
                     let piecesString = buildPieces pieces
                     (sizeString ++ passesString ++ piecesString)

-- | Build a given Int into a String
buildInt :: Int -- ^ Int to be converted
          -> String -- ^ Converted String
buildInt integer = show integer ++ "\n"

-- | Build a given Pieces list into a String
buildPieces :: [(Position, Col)] -- ^ List of Pieces to be converted
               -> String -- ^ Converted String
buildPieces [] = ""
buildPieces (((x,y), colour): qs) 
                = do let positionString = (show x) ++ "," ++ (show y)
                     let colourString = buildColour colour
                     positionString ++ "/" ++ colourString ++ (buildPieces qs)
    




