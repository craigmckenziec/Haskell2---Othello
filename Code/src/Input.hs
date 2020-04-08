module Input where

import Board
import AI
import Data.Char (isDigit)
import Data.Char(digitToInt)


-- Given an input string, convert it to a board coordinate as a pair of Ints
-- e.g. getCoord "D4" => (3,3) since coordinates are 0-based internally.
--   or getCoord "F2" => (5,1)
getCoord :: String -> (Int, Int)
getCoord move = if length(move) == 2
                then do let asciiValue = fromEnum (head move)
                        let reducedValue = asciiValue-65
                        let secondValue = last move
                        if isDigit secondValue == True
                        then do let secondValueInt = digitToInt(secondValue)
                                if reducedValue < 8 && secondValueInt < 8
                                    then (reducedValue, secondValueInt)
                                    else (-1, 0)
                        else (-1, 0)
                else (-1, 0)
