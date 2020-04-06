module Display where

import Board

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
showGameState :: GameState -> String
showGameState g = undefined

