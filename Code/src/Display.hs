module Display where

import Board
import Data.Char

{- Displaying the board. For example, you could render it as follows,
 - where 'O' is a white piece and '*' is a black piece:

   A B C D E F G H
 1 . . . . . . . .
 2 . . . . . . . .
 3 . . . . . . . .
 4 . . . O * . . .
 5 . . . * O . . .
 6 . . . . . . . .
 7 . . . . . . . .
 8 . . . . . . . .

 -}

-- Given a game state, return a String which represents the state of the
-- board.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
showGameState   :: GameState -> String
showGameState g = "   A" ++ (getLetter 0 (size (board g))) ++ (getRow 0 (size (board g)) (pieces (board g)))

getLetter                 :: Int -> Int -> String
getLetter x n | x == n-1  = ""
              | otherwise = [' ', chr(66+x)] ++ (getLetter (x+1) n)

getRow                        :: Int -> Int -> [(Position, Col)] -> String
getRow y n pieces | y == n    = []
                  | otherwise = "\n " ++ (show y) ++ (getPiece 0 y n pieces) ++ (getRow (y+1) n pieces)

getPiece                          :: Int -> Int -> Int -> [(Position, Col)] -> String
getPiece x y n pieces | x == n    = ""
                      | otherwise = " " ++ (findPiece x y pieces) ++ (getPiece (x+1) y n pieces)

findPiece                                                  :: Int -> Int -> [(Position, Col)] -> String
findPiece x y []                                           = "." --empty space character
findPiece x y (((x_, y_), colour):xs) | (x, y) == (x_, y_) = getPieceStr colour
                                      | otherwise          = findPiece x y xs

getPieceStr      :: Col -> String
getPieceStr Black = "*"
getPieceStr White = "0"
