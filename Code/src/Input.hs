module Input where

import Board
import AI
import Data.Char (isDigit)
import Data.Char(digitToInt)


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
